import Foundation

func performCmd<event>(_ cmd: Elm.PlatformCmd_Cmd<event>) {
    for cmdSingle in cmd {
        switch cmdSingle {
        case let .PlatformCmd_PortOutgoing(name: name, value: value):
            var maybeValueJsonDecodeError: Elm.JsonDecode_Error? = .none
            switch name {
            case "portStdOutWrite":
                switch Elm.JsonDecode_decodeValue(Elm.JsonDecode_string, value) {
                case let .Result_Ok(toWrite):
                    FileHandle.standardOutput.write(Data(toWrite.utf8))
                case let .Result_Err(jsonDecodeError):
                    maybeValueJsonDecodeError = .some(jsonDecodeError)
                }
            case "portProcessExit":
                switch Elm.JsonDecode_decodeValue(Elm.JsonDecode_int, value) {
                case let .Result_Ok(code):
                    exit(Int32(truncatingIfNeeded: Int(code)))
                case let .Result_Err(jsonDecodeError):
                    print(
                        "failed to decode value of port outgoing \(name): \(Elm.JsonDecode_errorToString(jsonDecodeError))"
                    )
                }
            case _:
                print("unknown port outgoing name \(name)")
            }
            if let valueJsonDecodeError = maybeValueJsonDecodeError {
                print(
                    "failed to decode value of port outgoing \(name): \(Elm.JsonDecode_errorToString(valueJsonDecodeError))"
                )
            }
        }
    }
}

func elmSubscriptionToDictionary(_ elmSub: Elm.PlatformSub_Sub<Elm.Main_Event>)
    -> [String: Elm.PlatformSub_SubSingle<Elm.Main_Event>]
{
    Dictionary(
        uniqueKeysWithValues: elmSub.map({ elmSubSingle in
            switch elmSubSingle {
            case let .PlatformSub_PortIncoming(name: name, onValue: _):
                (name, elmSubSingle)
            }
        }))
}

final actor ElmRuntimeActor {
    var elmCurrentState: Elm.Main_State
    var elmCurrentSubscriptions: [String: Elm.PlatformSub_SubSingle<Elm.Main_Event>]
    var elmSubscriptionsRunning: [String: Task<Void, Never>]
    var dispatchGroup: DispatchGroup
    init(_ elmCurrentState: Elm.Main_State) {
        self.elmCurrentState = elmCurrentState
        self.elmCurrentSubscriptions = Dictionary()
        self.elmSubscriptionsRunning = Dictionary()
        self.dispatchGroup = DispatchGroup()
        dispatchGroup.enter()
    }
    static func start(_ elmCurrentState: Elm.Main_State) async {
        let runtime: ElmRuntimeActor = ElmRuntimeActor(elmCurrentState)
        await runtime.updateSubscriptionsBasedOnElmCurrentState()
        // leaving out dispatchGroup.wait would stop without finishing tasks
        // created from updateSubscriptionsBasedOnElmCurrentState.
        // If you know of a better way to run until all tasks complete
        // please tell me <3
        // await runtime.dispatchGroup.wait()
        await runtime.dispatchGroup.wait()
    }

    func onEvent(_ event: Elm.Main_Event) {
        let updated: Elm.Tuple<Elm.Main_State, Elm.PlatformCmd_Cmd<Elm.Main_Event>> =
            Elm.Main_main.update(event)(self.elmCurrentState)
        performCmd(updated.second)
        self.elmCurrentState = updated.first
        self.updateSubscriptionsBasedOnElmCurrentState()
    }

    func updateSubscriptionsBasedOnElmCurrentState() {
        let updatedElmSubscriptions: [String: Elm.PlatformSub_SubSingle<Elm.Main_Event>] =
            elmSubscriptionToDictionary(Elm.Main_main.subscriptions(elmCurrentState))
        if updatedElmSubscriptions.isEmpty {
            dispatchGroup.leave()
        }
        let subsToRemove: [String: Elm.PlatformSub_SubSingle<Elm.Main_Event>] =
            Elm.Dict_diff(elmCurrentSubscriptions, updatedElmSubscriptions)
        let subsToAdd: [String: Elm.PlatformSub_SubSingle<Elm.Main_Event>] =
            Elm.Dict_diff(updatedElmSubscriptions, elmCurrentSubscriptions)
        for (key:subToCancelKey, value:_) in subsToRemove {
            elmSubscriptionsRunning[subToCancelKey]?.cancel()
            elmSubscriptionsRunning.removeValue(forKey: subToCancelKey)
        }
        elmCurrentSubscriptions = updatedElmSubscriptions
        for (subSingleKey, subSingle) in subsToAdd {
            elmSubscriptionsRunning[subSingleKey] =
                performSubSingle(subSingleKey, subSingle)
        }
    }
    func performSubSingle(
        _ subSingleKey: String,
        _ subSingle: Elm.PlatformSub_SubSingle<Elm.Main_Event>
    )
        -> Task<Void, Never>?
    {
        switch subSingle {
        case let .PlatformSub_PortIncoming(name: name, onValue: _):
            switch name {
            case "portStdInReadLine":
                return Task {
                    while true {
                        if Task.self.isCancelled {
                            return
                        } else {
                            // cancelling stdin reads seems to be an unsolved problem
                            // in the swift world. Now it might ask for input longer than necessary
                            switch readLine() {
                            case .none: return
                            case let .some(input):
                                if Task.self.isCancelled {
                                    return
                                } else {
                                    switch self.elmCurrentSubscriptions[subSingleKey] {
                                    case .none: return  // failed to associate
                                    case let .some(
                                        .PlatformSub_PortIncoming(name: _, onValue: onValue)
                                    ):
                                        self.onEvent(
                                            onValue(Elm.JsonEncode_string(input)))
                                    }
                                }
                            }
                        }
                    }
                }
            case _:
                print("unknown port incoming name \(name)")
                return .none
            }
        }
    }
}

let elmInitialized: Elm.Tuple<Elm.Main_State, Elm.PlatformCmd_Cmd<Elm.Main_Event>> =
    Elm.Main_main.init_(Elm.Array_toList(Array(CommandLine.arguments.dropFirst())))
performCmd(elmInitialized.second)
await ElmRuntimeActor.start(elmInitialized.first)
