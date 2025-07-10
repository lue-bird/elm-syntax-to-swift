module Age exposing (toString)

toString : Int -> String
toString age =
    String.fromInt age ++ " years old"
