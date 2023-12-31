# WCOL

Parse and validate "Willow's Content Organization Language", a simple markup language.

* A WCOL document contains frontmatter and a list of sections and groups.
* Frontmatter is text that appears before the start of the first section or group.
* See [the test document](../test.wcol) for an example of what WCOL looks like.
* A block begins with a `[header]`, which can contain arguments.
  * Arguments can either be singlets or pairs of strings, like `foo` or `bar=baz`.
  * A string can be `bare`, `"quoted"`, or `(parenthesized)`, and these can have special meanings.
    * In WCMS, for example, a parenthesized singlet on a visual block represents alt text.
  * The name of a block can also be formatted anyway a string can, like `[(a parenthesized block) "with" (two)=arguments]`.
  * Arguments are non-ordered and they will be sorted alphabetically during parsing.
  * After the header, any valid UTF-8 can follow, but it must not contain a left square bracket preceded by two newlines. If you must include something like this, put a space on the empty line to prevent it from being treated as a new block.
* A group begins with a `[+header]`, which is identical to a block header except that it starts with a plus, and a `[-header]`, which cannot take arguments.
  * Blocks and groups can occur inside of a group.
  * Groups cannot have frontmatter.
