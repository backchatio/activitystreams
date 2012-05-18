package io.backchat.jsonschema

import org.mozilla.javascript.{ScriptableObject, Context}


object EcmaRegex {

  private[this] val JsScript =
    """
      |function regexIsValid(re) {
      |  try {
      |    new RegExp(re);
      |    return true;
      |  } catch (e) {
      |    return false;
      |  }
      |}
      |
      |function regMatch(re, input) {
      |  return new RegExp(re).test(input);
      |}
    """.stripMargin



  def withContext[T](thunk: (Context, ScriptableObject) => T) = try {
    val context = Context.enter()
    val scope = context.initStandardObjects()
    context.evaluateString(scope, JsScript, "re", 1, null)
    thunk(context, scope)
  } finally { Context.exit() }

  /**
   * Validate that a regex is correct
   *
   * @param regex the regex to validate
   * @return true if the regex is valid
   */
  def isValid(regex: String): Boolean = withContext { (context, scope) =>
    val regIsValid = scope.get("regexIsValid", scope).asInstanceOf[org.mozilla.javascript.Function]
    regIsValid.call(context, scope, scope, Array(regex)).asInstanceOf[Boolean]
  }

  /**
   * Matches an input against a given regex, in the <b>real</b> sense
   * of matching, that is, the regex can match anywhere in the input.
   * Java's java.util.regex makes the unfortunate mistake to make
   * people believe that matching is done on the whole input... Which is
   * not true.
   *
   * Also note that the regex MUST have been validated at this point
   * (using [[#regexIsValid(String)]]).
   *
   * @param regex the regex to use
   * @param input the input to match against (and again, see description)
   * @return true if the regex matches the input
   */
  def matches(regex: String, input: String): Boolean = withContext { (context, scope) =>
    val regMatch = scope.get("regMatch", scope).asInstanceOf[org.mozilla.javascript.Function]
    regMatch.call(context, scope, scope, Array(regex, input)).asInstanceOf[Boolean]
  }
}
