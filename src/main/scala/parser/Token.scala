package parser



case class Token(
                  tpe: Token.Type,
                  text: String,
                  startPos: Int
                )

object Token:
  enum Type:
    case IntLiteral
    case Plus
    case Times
    case Identifier
    case True
    case False
    case EOF

