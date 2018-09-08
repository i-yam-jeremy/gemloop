/*

	A library for parsing a simple language

*/
const SimpleParser = (() => {

	/*
		The lexer/tokenizer for the language
		Converts an input string into a stream of tokens
	*/
	const Lexer = (() => {

		/* The reserved keywords that cannot be used as identifiers */
		const KEYWORDS = [
			"if", "elif", "else",
			"true", "false"
		];

		/* The regular expressions for matching tokens */
		const TOKEN_REGEXES = {
			"number": /^(?:-?[0-9]*\.[0-9]+)|^(?:-?[0-9]+)/,
			"identifier": /^[A-Za-z_][A-Za-z_0-9]*/,
			"(": /^\(/,
			")": /^\)/,
			"[": /^\[/,
			"]": /^\]/,
			"{": /^\{/,
			"}": /^\}/,
			"+": /^\+/,
			"-": /^\-/,
			"/": /^\//,
			"*": /^\*/,
			"%": /^\%/,
			"<": /^\</,
			">": /^\>/,
			".": /^\./,
			",": /^\,/,
			":": /^\:/,
			";": /^\;/,
			"\"": /^\"/,
			"'": /^\'/,
			"!": /^\!/,
			"=": /^\=/,
			"\\": /^\\/,
			"|": /^\|/,
			"&": /^\&/,
			"whitespace": /^\s+/
		};

		/* The token data container class */
		class Token {
			/*
				Creates a token with the given data
				@param type - string - the type of the token
				@param value - string - the string value of the token
				@param line - positive integer - the line number at the start of this token
				@param col - positive integer - the column number at the start of this token
				@param charPosition - natural number - the offset in characters from the start 
					of the source string to the start of this token
			*/
			constructor(type, value, line, col, charPosition) {
				this.type = type;
				this.value = value;
				this.line = line;
				this.col = col;
				this.charPosition = charPosition;
			}
		}

		/* The stream of tokens */
		class TokenStream {

			/*
				Fields:

				tokens - Token[] - the source array of tokens
				position - natural_number - the current position in the token stream
				savedStates - (natural number)[] - the saved position states, used to restore to a previous position state
				source - string - the source code string
			*/

			/* 
				Creates a token stream from the given array of tokens
				@param tokens - Token[] - the source array of tokens
				@param source - string - the source code string
			*/
			constructor(tokens, source) {
				this.tokens = tokens;
				this.position = 0;
				this.savedStates = [];
				this.source = source;

				this.__preprocess();
			}

			/*
				Preprocess whitespace if it is available at the current position
				@param processedTokens - Token[] - the output array of already processed tokens
				@return boolean - true iff whitespacce was found and added to processedTokens, otherwise false
			*/
			__preprocessWhitespace(processedTokens) {
				if (this.tokens[this.position].type == "whitespace") {
					this.position++; // skip
					return true;
				}
				return false;
			}

			/*
				Preprocess a string literal if one is available at the current position
				@param processedTokens - Token[] - the output array of already processed tokens
				@return boolean - true iff a string literal was found and added to processedTokens, otherwise false
			*/
			__preprocessStringLiteral(processedTokens) {
				if (["'", "\""].indexOf(this.tokens[this.position].type) > -1) { // string
					let quoteType = this.tokens[this.position].type;
					this.position++; // skip initial ' or "
					let escaped = false;
					let stringValue = [];
					while (this.tokens[this.position].type != quoteType || escaped) {
						if (!escaped && this.tokens[this.position].type == "\\") {
							escaped = true;
						}
						else {
							if (escaped) {
								escaped = false;
								const escapeChars = {
									"n": "\n",
									"r": "\r",
									"t": "\t",
									"'": "'",
									"\"": "\"",
									"\\": "\\"
								};
								let escapeChar = this.tokens[this.position].value;
								if (escapeChar in escapeChars) {
									stringValue += escapeChars[escapeChar];
								}
								else {
									throw "Invalid escape char: " + escapeChar;
								}
							}
							else {
								stringValue += this.tokens[this.position].value;
							}
						}
						this.position++;
					}
					this.position++; // skip end quote
					processedTokens.push(new Token("string", stringValue)); 
					return true;
				}
				return false;
			}

			/*
				Preprocess a comment if one is available at the current position
				@param processedTokens - Token[] - the output array of already processed tokens
				@return boolean - true iff a comment was found and added to processedTokens, otherwise false
			*/
			__preprocessComment(processedTokens) {
				if (this.tokens[this.position].type == "/" && this.tokens[this.position+1] && ["/", "*"].indexOf(this.tokens[this.position+1].type) > -1) { // check if comment
					let bodyTokens = [];
					if (this.tokens[this.position+1].type == "/") { // single-line comment
						this.position += 2; // skip "//"
						while (!(this.position >= this.tokens.length || this.tokens[this.position].value.includes("\n"))) {
							bodyTokens.push(this.tokens[this.position]);
							this.position++;
						}
						this.position++; // skip the last whitespace
					}
					else if (this.tokens[this.position+1].type == "*") { // multi-line comment
						this.position += 2; // skip "/*"
						while (!(this.tokens[this.position].type == "*" && this.tokens[this.position+1] && this.tokens[this.position+1].type == "/")) {
							if (this.position >= this.tokens.length) {
								throw "Reached end of file with unclosed multi-line comment";
							}
							bodyTokens.push(this.tokens[this.position]);
							this.position++;
						}
						this.position += 2; // skip the end "*/"
					}
					
					let body = bodyTokens.map(token => token.value).join("");
					processedTokens.push(new Token("comment", body));
					return true;
				}
				return false;
			}

			/*
				Preprocess a keyword if one is available at the current position
				@param processedTokens - Token[] - the output array of already processed tokens
				@return boolean - true iff a keyword was found and added to processedTokens, otherwise false
			*/
			__preprocessKeyword(processedTokens) {
				let token = this.tokens[this.position];
				if (token.type == "identifier" && KEYWORDS.indexOf(token.value) > -1) {
					processedTokens.push(new Token("keyword", token.value));
					this.position++;
					return true;
				}
				return false;
			}

			/*
				Preprocessed the tokens, removing whitespace, condensing string literals into a single token, and condensing comments into a single token
			*/
			__preprocess() {
				const preprocessors = [
					a => this.__preprocessWhitespace(a),
					a => this.__preprocessStringLiteral(a),
					a => this.__preprocessComment(a),
					a => this.__preprocessKeyword(a)
				];
				let processedTokens = [];
				while (this.position < this.tokens.length) {
					let specialPreprocessUsed = false; // if any of the token modifiers are used
					for (let preprocessor of preprocessors) {
						specialPreprocessUsed |= preprocessor(processedTokens);
						if (this.position >= this.tokens.length) {
							break;
						}
					}
					if (!specialPreprocessUsed) {
						processedTokens.push(this.tokens[this.position++]);
					}
				}
				this.position = 0;
				this.tokens = processedTokens;
			}

			/*
				Gets the next token in the token stream
				@param keepComments - boolean (Optional) - whether or not to read a comment if it is the next token (useful for using comments above functions as documentation)
				@return Token - the next token in the stream, or a Token with type "EOF" if reached the end of the stream
			*/
			next(keepComments) {
				if (this.position >= this.tokens.length) {
					return new Token("EOF", "");
				}
				else {
					if (!keepComments && this.tokens[this.position].type == "comment") {
						this.position++;
						return this.next();
					}
					else {
						return this.tokens[this.position++];
					}
				}
			}

			/* saves the current position into the stack of states */
			save() {
				this.savedStates.push(this.position);
			}

			/* restores back to the most recently saved position state */
			restore() {
				this.position = this.savedStates.pop();
			}

			/* clears the last saved state without restoring to it */
			clearSave() {
				this.savedStates.pop();
			}

			/*
				Displays an error at the current token
				@param message - string - the error message
				@throws Exception - an exception based on the given error message and token position
			*/
			error(message) {
				let positionString;
				let charPosition;
				if (this.position >= this.tokens.length) {
					positionString = "EOF";
					charPosition = Math.max(0, this.source.length-20);
				}
				else {
					let token = this.tokens[this.position];
					positionString = token.line + ":" + token.col;
					charPosition = token.charPosition;
				}
				throw "Error occurred at " + positionString + " around " + this.source.substring(charPosition, charPosition+20) + "\n" + message;
			}
		}

		/*
			Converts the input string into a stream of tokens
			@param s - string - the input string
			@return TokenStream - the stream of tokens
		*/
		function lex(s) {
			let source = s;
			let tokens = [];
			let line = 1;
			let col = 1;
			let charPosition = 0;
			while (s.length > 0) {
				let foundToken = false;
				for (let tokenType in TOKEN_REGEXES) {
					let result = TOKEN_REGEXES[tokenType].exec(s);
					if (result && result[0] != null) {
						let value = result[0];
						tokens.push(new Token(tokenType, value, line, col, charPosition));
						foundToken = true;
						s = s.substring(value.length);
						charPosition += value.length;
						let lineSeparated = value.split("\n");
						line += lineSeparated.length-1; // count how many newlines there were in the token
						if (lineSeparated.length == 1) { // if no newlines
							col += value.length;
						}
						else { // if newlines
							col = 1 + lineSeparated[lineSeparated.length-1].length; // 1 + (length of last line) because indexing starts at 1
						}
						break;
					}
				}
				if (!foundToken) {
					throw "Unidentitifed token at " + line + ":" + col + ": " + s;
				}
			}
			return new TokenStream(tokens, source);
		}

		return {
			lex
		};

	})();

	/*
		Parses expressions from the token stream
	*/
	const ExprParser = (() => {

		/* The expression data container class */
		class Expr {
			/*
				Creates an expression from the given data
				@param type - string - the expression type
				@param data - any - the data associated with this expression
			*/
			constructor(type, data) {
				this.type = type;
				this.data = data;
			}
		}

		/*
			Attempts to parse an expression enclosed in parentheses
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches a parenthetical expr, otherwise false
		*/
		function parentheses(tokens) {	
			tokens.save();
			let nextToken = tokens.next();
			if (nextToken.type == "(") {
				let expr = parseExpr(tokens);
				let endToken = tokens.next();
				if (endToken.type == ")") {
					tokens.clearSave();
					return expr;
				}
				else {
					tokens.error("Expected ')' but found '" + endToken.type + "'");
				}
			}
			else {
				tokens.restore();
				return false;
			}
		}

	
		/*
			Attempts to parse a boolean literal expression
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches a boolean literal expr, otherwise false
		*/
		function booleanLiteral(tokens) {
			tokens.save();
			let nextToken = tokens.next();
			if (nextToken.type == "keyword" && ["true", "false"].indexOf(nextToken.value) > -1) {
				tokens.clearSave();
				return new Expr("literal", nextToken.value == "true");
			}
			else {
				tokens.restore();
				return false;
			}
		}
		
		/*
			Attempts to parse a number literal expression
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches a number literal expr, otherwise false
		*/
		function numberLiteral(tokens) {
			tokens.save();
			let nextToken = tokens.next();
			if (nextToken.type == "number") {
				tokens.clearSave();
				if (nextToken.value.includes(".")) {
					return new Expr("literal", parseFloat(nextToken.value));
				}
				else {
					return new Expr("literal", parseInt(nextToken.value));
				}
			}
			else {
				tokens.restore();
				return false;
			}
		}
		
		/*
			Attempts to parse a string literal expression
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an string literal expr, otherwise false
		*/
		function stringLiteral(tokens) {
			tokens.save();
			let nextToken = tokens.next();
			if (nextToken.type == "string") {
				tokens.clearSave();
				return new Expr("literal", nextToken.value);
			}
			else {
				tokens.restore();
				return false;
			}
		}

		/*
			Attempts to parse a variable name expression (a non-keyword identifier)
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches a variable name expr, otherwise false
		*/
		function variableName(tokens) {
			tokens.save();
			let nextToken = tokens.next();
			if (nextToken.type == "identifier") {
				tokens.clearSave();
				return new Expr("variable", nextToken.value);
			}
			else {
				tokens.restore();
				return false;
			}
		}

		/*
			Attempts to parse an atomic expression
			An Atomic Expression is one of:
				Parentheses Expression,
				Integer Literal Expression,
				Variable Name Expression
			It is called an atomic expression because it is an expression that cannot be broken down into further sub-expressions, it is the smallest unit, hence "atomic".
			Note: Parenthetical expressions can be broken down further but they are bundled with the other atomic expressions because they share the same precedence (the highest)
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an atomic expr, otherwise false
		*/
		function atom(tokens) {
			let exprParsers = [parentheses, booleanLiteral, numberLiteral, stringLiteral, variableName];
			for (let parser of exprParsers) {
				let expr = parser(tokens);
				if (expr) {
					return expr;
				}
			}
			return false;	
		}

		/*
			Attempts to parse a unary operator expression
			A Unary Operator Expression is one of:
				Atomic Expression,
				"-" (Atomic Expression),
				"!" (Atomic Expression)
			@param tokens - TokenStream - the stream of tokens
			@return Expr? - the expr if it matches a unary operator expr, otherwise false
		*/
		function unaryOp(tokens) {
			tokens.save();
			let token = tokens.next();
			if (["-", "!"].indexOf(token.type) > -1) {
				let expr = atom(tokens);
				if (!expr) {
					tokens.error("Unary Operator: expected expression but found none");
				}
				return new Expr("unary-op", {expr: expr, op: token.type});
			}
			else {
				tokens.restore();
				return atom(tokens);
			}
		}

		/*
			Attempts to parse a lambda expression
			A Lambda Expression is one of:
				Unary Operator Expression,
				"() => {" Expression "}"
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if it matches a lambda expr, otherwise false
		*/
		function lambda(tokens) {
			tokens.save();
			let paramNames = [];
			let firstToken = tokens.next();
			if (firstToken.type == "identifier") { // for single parameter function with no parentheses
				paramNames.push(firstToken.value);
			}
			else if (firstToken.type == "(") { // parentheses with multiple args
				while (true) {
					let paramToken = tokens.next();
					if (paramToken.type == "identifier") {
						paramNames.push(paramToken.value);
					}
					else if (paramNames.length == 0 && paramToken.type == ")") { // to support functions with 0 arguments
						break;
					}

					let separatorToken = tokens.next();
					if (separatorToken.type == ",") {
						continue;
					}
					else if (separatorToken.type == ")") {
						break;
					}
					else { // was parenthetical atomic expression, not lambda expression, so restore and parse that instead
						tokens.restore();
						return unaryOp(tokens);
					}
				}
			}
			else {
				tokens.restore();
				return unaryOp(tokens);
			}

			if (!(tokens.next().type == "=" && tokens.next().type == ">")) { // if "=>" not found
				tokens.restore();
				return unaryOp(tokens);
			}

			if (tokens.next().type == "{") {
				let body = parseExpr(tokens);
				if (!body) {
					tokens.error("Lambda: no body found");
				}
				tokens.save(); // in case next token is not "}"
				if (tokens.next().type == "}") {
					tokens.clearSave(); // it was "}" so no need to restore
					tokens.clearSave();
					return new Expr("lambda", {params: paramNames, body: body});
				}
				else {
					tokens.restore();
					tokens.error("Lambda: expected '}' after body but none found");
				}
			}
			else {
				tokens.restore();
				return unaryOp(tokens);
			}
		}

		/*
			Attempts to parse a class definition expression
			A Class Definition Expression is one of:
				Lambda Expression,
				"<>" (":" Expression)? "{" (Method Definition)* "}"
			where a Method Definition is:
				(Method Name) (Lambda Expression)
			Note: in the above case Lambda Expression must be of type "lambda", it can not be the fall-through atomic case,
				this is to ensure the method definition is a function and not any arbitrary expression
			For example:
				<>:parentClass {
					myMethod(param1) => {
						x = param1*2
					}
				}
			@param tokens - TokenStream - the token stream
			@return Expr? - an expr if it matches a class definition expr, otherwise false

		*/
		function classDef(tokens) {
			tokens.save();
			if (tokens.next().type == "<" && tokens.next().type == ">") { // match initial "<>"
				let nextToken = tokens.next();
				let parentClassExpr;
				if (nextToken.type == ":") {
					console.log("Hi");
					parentClassExpr = parseExpr(tokens);
					if (!parentClassExpr) {
						tokens.error("Class Definition: expected parent class expression");
					}
					nextToken = tokens.next(); // for "{"
				}
				if (nextToken.type != "{") {
					tokens.restore();
					return lambda(tokens);
				}
				let methods = {};
				while (tokens.save(), tokens.next().type != "}") { // save in case next token is not a "}" so it can be restored and method definition read
					tokens.restore(); // restore since next token was not "}"
					tokens.save(); // save in case token is not identifier
					let methodNameToken = tokens.next();
					if (methodNameToken.type != "identifier") {
						tokens.restore();
						tokens.error("Class Definition: expected method name, but found token of type '" + methodNameToken.type + "'");
					}
					tokens.clearSave(); // clear save since it was identifier
					let methodName = methodNameToken.value;
					tokens.save(); // save in case expr is not type lambda
					let method = lambda(tokens); // parse lambda expression
					if (!method) {
						tokens.restore();
						tokens.error("Class Definition: expected lambda expression for method but found none");
					}
					if (method.type != "lambda") { // if next is not a lambda expression, restore and fall-through since it is not a class definition or there is an error
						tokens.restore();
						tokens.error("Class Definition: expected lambda expression for method but found '" + method.type + "'");
					}
					tokens.clearSave(); // clear save since it was type lambda
					methods[methodName] = method;
				}
				tokens.clearSave(); // matched a "}" so don't need to restore
				return new Expr("class-def", {parentClass: parentClassExpr, methods: methods});
			}
			else {
				tokens.restore();
				return lambda(tokens);
			}
		}

		/*
			Attempts to parse a function call or object field expression
			A Function Call or Object Field Expression is one of:
				Class Definition Expression,
				{
					func: Class Definition Expression,
					args: Expression[]
				},
				{
					func: Function Call or Object Field Expression,
					args: Expression[]
				}
			Note: these two expression types are in the same function because they have equal precedence and can be interlaced, for example obj.getBounds(10).width
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches a function call expr, otherwise false
		*/
		function functionCallOrObjectField(tokens) {
			tokens.save();
			let expr = classDef(tokens);
			if (!expr) {
				tokens.restore();
				return false;
			}

			let type;
			while (tokens.save(), ["(", "."].indexOf((type = tokens.next().type)) > -1) { // save for when next token is not '(', and loop so multiple function calls can be strung together like f()(x)(y)
				tokens.clearSave(); // '(' or '.' is valid so don't need to save it because it does not need to be undone later
				if (type == "(") { // parse function call expr
					tokens.save(); // save in case token is not ")"
					if (tokens.next().type == ")") { // handle no-argument function calls
						expr = new Expr("function-call", {func: expr, args: []});	
					}
					else {
						tokens.restore(); // was not ")" so restore
						let args = [];
						while (true) {
							let arg = assignment(tokens); // cannot be joint expression because it is syntactically ambiguous with comma separated args (to use joint expression wrap in parentheses)
							if (!arg) {
								tokens.error("Function Call: expected argument expression but none found");
							}
							args.push(arg);
							let commaToken = tokens.next();
							if (commaToken.type == ",") {
								continue;
							}
							else if (commaToken.type == ")") {
								break;
							}
							else {
								tokens.error("Function Call: expected ',' or ')' but found '" + commaToken.type + "'");
							}
						}
						expr = new Expr("function-call", {func: expr, args: args});
					}
				}
				else if (type == ".") { // parse object field expr
					let fieldName = variableName(tokens);
					if (!fieldName) {
						tokens.error("Object Field: expected field name but found none");
					}
					expr = new Expr("object-field", {object: expr, field: fieldName.data});
				}
			}
			tokens.restore(); // undo last request of token because it was not a '('
			tokens.clearSave();
			return expr;
		}

		/*
			Attempts to parse a multiplication, division, or modulo expression
			An Multiplication, Division, or Modulo Expression is one of:
				Function Call or Object Field Expression,
				{
					left: Function Call or Object Field Expression,
					right: Function Call or Object Field Expression
				},
				{
					left: Multiplication, Division, or Modulo Expression,
					right: Function Call or Object Field Expression
				} 
			Note: parses as many as possible if in sequence, giving precedence (closer to leaves of expr tree) to operators the occur first when read left to right
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an multiplication, division, or modulo expr, otherwise false
		*/
		function mulDivMod(tokens) {
			let expr = functionCallOrObjectField(tokens);
			let op;
			while (tokens.save(), ["*", "/", "%"].indexOf((op = tokens.next().type)) > -1) { // save before the operator token request in case it is not one of '*', '/', '%'
				tokens.clearSave(); // it is a valid operator so save can be cleared
				let rhs = functionCallOrObjectField(tokens); // right-hand side
				if (!rhs) {
					tokens.error("Arithmetic Operator: expected right-hand side expression for operator '" + op + "'");
				}
				expr = new Expr(op, {left: expr, right: rhs});
			}
			tokens.restore(); // undo the last next token request because it was not an operator
			return expr;
		}

		/*
			Attempts to parse an addition or subtraction expression
			An Addition or Subtraction Expression is one of:
				Multiplication or Division Expression,
				{
					left: Multiplication, Division, or Modulo Expression,
					right: Multiplication, Division, or Modulo Expression
				},
				{
					left: Addition or Subtraction Expression,
					right: Multiplication, Division, or Modulo Expression
				} 
			Note: parses as many as possible if in sequence, giving precedence (closer to leaves of expr tree) to operators the occur first when read left to right
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an addition or subtraction expr, otherwise false
		*/
		function addSub(tokens) {
			let expr = mulDivMod(tokens);
			let op;
			while (tokens.save(), ["+", "-"].indexOf((op = tokens.next().type)) > -1) { // save before the operator token request in case it is not one of '+', '-'
				tokens.clearSave(); // it is a valid operator so save can be cleared
				let rhs = mulDivMod(tokens); // right-hand side
				if (!rhs) {
					tokens.error("Arithmetic Operator: expected right-hand side expression for operator '" + op + "'");
				}
				expr = new Expr(op, {left: expr, right: rhs});
			}
			tokens.restore(); // undo the last next token request because it was not an operator
			return expr;
		}
		
		/*
			Attempts to parse a comparison expression
			A Comparison Expression is one of:
				Addition or Subtraction Expression,
				{
					left: Addition or Subtraction Expression,
					right: Addition or Subtraction Expression
				},
				{
					left: Comparison Expression,
					right: Addition or Subtraction Expression
				} 
			Note: parses as many as possible if in sequence, giving precedence (closer to leaves of expr tree) to operators the occur first when read left to right
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches a comparison expr, otherwise false
		*/
		function comparison(tokens) {
			let expr = addSub(tokens);
			let op;
			while (tokens.save(), ["<", ">"].indexOf((op = tokens.next().type)) > -1 || ["<=", ">=", "!=", "=="].indexOf((op += tokens.next().type)) > -1) { // save before the operator token request in case it is not one of the valid operators
				tokens.clearSave(); // it is a valid operator so save can be cleared
				let rhs = addSub(tokens); // right-hand side
				if (!rhs) {
					tokens.error("Comparison Operator: expected right-hand side expression for operator '" + op + "'");
				}
				expr = new Expr(op, {left: expr, right: rhs});
			}
			tokens.restore(); // undo the last next token request because it was not an operator
			return expr;
		}

		/*
			Attempts to parse a logical operator expression
			A Logical Operator Expression is one of:
				Comparison Expression,
				{
					left: Comparison Expression,
					right: Comparison Expression
				},
				{
					left: Logical Operator Expression,
					right: Comparison Expression
				} 
			Note: parses as many as possible if in sequence, giving precedence (closer to leaves of expr tree) to operators the occur first when read left to right
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches a logical operator expr, otherwise false
		*/
		function logicalOp(tokens) {
			let expr = comparison(tokens);
			let op;
			while (tokens.save(), ["&&", "||"].indexOf((op = tokens.next().type+tokens.next().type)) > -1) { // save before the operator token request in case it is not one of the valid operators
				tokens.clearSave(); // it is a valid operator so save can be cleared
				let rhs = comparison(tokens); // right-hand side
				if (!rhs) {
					tokens.error("Logical Operator: expected right-hand side expression for operator '" + op + "'");
				}
				expr = new Expr(op, {left: expr, right: rhs});
			}
			tokens.restore(); // undo the last next token request because it was not an operator
			return expr;
		}

		/*
			Attempts to parse an if expression
			An If Expression is one of:
				Logical Operator Expression,
				"if" (Atomic Expression) "{" Expression "}" ("elif" (Atomic Expression) "{" Expression "}")* ("else" "{" Expression "}")?
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an if expr, otherwise false
		*/
		function ifElse(tokens) {
			tokens.save();
			let conditionals = [];
			let keyword;
			while (tokens.save(), keyword = tokens.next(), keyword.type == "keyword" &&
				((conditionals.length == 0 && keyword.value == "if") ||
				 (conditionals.length > 0 && ["elif", "else"].indexOf(keyword.value) > -1))) {
				tokens.clearSave(); // keyword matched so no need to restore
				let conditionExpr = null;
				if (["if", "elif"].indexOf(keyword.value) > -1) {
					conditionExpr = atom(tokens);
					if (!conditionExpr) {
						tokens.error("Branch: expected condition expression but found none");
					}
				}

				tokens.save(); // in case next token is not "{"
				let token = tokens.next();
				if (token.type == "{") {
					tokens.clearSave(); // clear save since it was "{"
					let body = parseExpr(tokens);
					if (!body) {
						tokens.error("Branch: expected body but found none");
					}
					tokens.save(); // in case token is not "}"
					let token = tokens.next();
					if (token.type == "}") {
						tokens.clearSave(); // clear save since it was "}"
						conditionals.push({condition: conditionExpr, body: body});
					}
					else {
						tokens.restore();
						tokens.error("Branch: expected '}' but found token of type '" + token.type + "'");
					}
				}
				else {	
					tokens.restore();
					tokens.error("Branch: expected '{' but found token of type '" + token.type + "'");
				}
				
			}
			tokens.restore(); // restore last unmatched keyword attempt
			if (conditionals.length > 0) {
				return new Expr("if", {conditionals: conditionals});
			}
			else {
				tokens.restore();
				return logicalOp(tokens);
			}
		}

		/*
			Attempts to parse an assignment expression
			An Assignment Expression is one of:
				If Expression,
				Variable Expression "=" If Expression
			Note: the value of an assignment expr is the value of the right-hand side expression
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an assignment expr, otherwise false
		*/
		function assignment(tokens) {
			tokens.save();
			let lhs = ifElse(tokens); // left-hand side
			if (!lhs) {
				tokens.restore();
				return false;
			}
			if (["variable", "object-field"].indexOf(lhs.type) == -1) {
				return lhs;
			}
			if (tokens.next().type != "=") {
				tokens.restore();
				return ifElse(tokens);
			}
			let rhs = assignment(tokens); // right-hand side
			
			if (!rhs) {
				tokens.error("Assignment: expected expression on right-hand side of '=' but found none");
			}			

			tokens.clearSave();
			return new Expr("assignment", {variable: lhs, value: rhs});
		}

		/*
			Attempts to parse a joint expression
			A Joint Expression is:
				(Assignment Expression) ("," Assigment Expression)*
			Note: the value of the joint expr is the value of the last expression
			@param tokens - TokenStream - the stream of tokens
			@return Expr? - the expr if the token stream matches a joint expr, otherwise false
		*/
		function jointExpression(tokens) {
			let exprs = [assignment(tokens)];
			while (tokens.save(), tokens.next().type == ",") { // save in case token is not ","
				tokens.clearSave(); // the token was "," so no need to restore
				let nextExpr = assignment(tokens);
				if (!nextExpr) {
					tokens.error("Joint: expected another expression but found none");
				}
				exprs.push(nextExpr);
			}
			tokens.restore(); // restore since last token was not ","
			if (exprs.length > 1) { // if multiple joined expressions
				return new Expr("joint", {exprs: exprs});
			}
			else { // if only one expression, fall-through
				return exprs[0];
			}
		}

		/*
			Attempts to parse an expr from the given token stream
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an expr, otherwise false
		*/
		function parseExpr(tokens) {
			return jointExpression(tokens);
		}

		return {
			parse: parseExpr
		};

	})();

	/*
		Parse an input string
		@param s - string - the input source string
		@return Statement[] - the statements described by the source
	*/
	function parse(s) {
		const tokens = Lexer.lex(s);
		return ExprParser.parse(tokens);
	}

	return {
		parse
	};


})();

/*
	Evaluates the code in the simple language
*/
const SimpleInterpreter = (() => {

	/*
		A function object in the language
	*/
	class Func {
		
		/*
			Fields:

			params - string[] - the names of this function's parameters
			body - Expr - the body of the function
			scope - Scope - the scope in which the function was defined
		*/

		/*
			Creates a function from the given parameters, body, and scope
			@param params - string[] - the names of this function's parameters
			@param body - Expr - the body of the function
			@param scope - Scope - the scope in which the function was defined
		*/
		constructor(params, body, scope) {
			this.params = params;
			this.body = body;
			this.scope = scope;
		}

		/*
			Evaluates this function with the given arguments
			@param args - any[] - the arguments to be passed to the function
			@return any - the result of calling the function
		*/
		call(args) {
			if (this.params.length != args.length) {
				throw "Expected " + this.params.length + " arguments but found " + args.length;
			}
			let newScope = Object.assign({}, this.scope);
			for (let i = 0; i < this.params.length; i++) {
				newScope[this.params[i]] = args[i];
			}
			return evalExpr(this.body, newScope);
		}
	}

	/*
		A class definition in the language
		Note: this is not an instance of the class, it is the definition of the class
	*/
	class Class {

		/*
			Fields:

			parentClass - Class (Optional) - the parent class if there is one, otherwise undefined
			methods - Map<string, Lambda Expression> - method names mapped to the methods
			scope - Scope - the scope in which this class was defined
		*/

		/*
			Creates a class definition from the given parentClass, methods, and scope
			@param parentClass - Class (Optional) - the parent class if there is one, otherwise undefined
			@param methods - Map<string, Lambda Expression> - method names mapped to the methods
			@param scope - Scope - the scope in which this class was defined
		*/
		constructor(parentClass, methods, scope) {
			this.parentClass = parentClass;
			this.methods = methods;
			this.scope = scope;
		}

		/*
			Instantiates and object that is an instance of this class
			@param args - any[] - the arguments to the constructor (the init method)
			@return - object - an object that is an instance of this class
		*/
		instantiate(args) {
			let instance = {};
			let clazz = this;
			while (clazz != undefined) {
				let newScope = Object.assign({}, clazz.scope);
				newScope["this"] = instance;
				for (let methodName in clazz.methods) {
					let method = clazz.methods[methodName];
					if (!(methodName in instance)) {
						instance[methodName] = new Func(method.data.params, method.data.body, newScope);
					}
				}
				clazz = clazz.parentClass;
			}
			if ("init" in instance) {
				instance.init.call(args);
			}
			else {
				throw "Error no constructor found";
			}
			return instance;
		}
	}

	/*
		The evaluator functions for each type of expression
		Each function has the following signature: (Expression, Scope) => any,
			where scope is a dictionary mapping variable names to values
	*/
	const EXPR_EVALUATORS = {
		"literal": (e, s) => e.data,
		"variable": (e, s) => s[e.data],
		"+": (e, s) => evalExpr(e.data.left, s) + evalExpr(e.data.right, s),
		"-": (e, s) => evalExpr(e.data.left, s) - evalExpr(e.data.right, s),
		"*": (e, s) => evalExpr(e.data.left, s) * evalExpr(e.data.right, s),
		"/": (e, s) => evalExpr(e.data.left, s) / evalExpr(e.data.right, s),
		"%": (e, s) => evalExpr(e.data.left, s) % evalExpr(e.data.right, s),
		">": (e, s) => evalExpr(e.data.left, s) > evalExpr(e.data.right, s),
		"<": (e, s) => evalExpr(e.data.left, s) < evalExpr(e.data.right, s),
		">=": (e, s) => evalExpr(e.data.left, s) >= evalExpr(e.data.right, s),
		"<=": (e, s) => evalExpr(e.data.left, s) <= evalExpr(e.data.right, s),
		"!=": (e, s) => evalExpr(e.data.left, s) != evalExpr(e.data.right, s),
		"==": (e, s) => evalExpr(e.data.left, s) == evalExpr(e.data.right, s),
		"&&": (e, s) => evalExpr(e.data.left, s) && evalExpr(e.data.right, s),
		"||": (e, s) => evalExpr(e.data.left, s) || evalExpr(e.data.right, s),
		"unary-op": (e, s) => {
			switch (e.data.op) {
				case "-":
					return -evalExpr(e.data.expr, s);
				case "!":
					return !evalExpr(e.data.expr, s);
			}
		},
		"lambda": (e, s) => {
			let newScope = Object.assign({}, s);
			return new Func(e.data.params, e.data.body, newScope);
		},
		"class-def": (e, s) => {
			let parentClass = e.data.parentClass && evalExpr(e.data.parentClass, s);
			return new Class(parentClass, e.data.methods, s);
		},
		"function-call": (e, s) => {
			let func = evalExpr(e.data.func, s);
			let argValues = e.data.args.map(argExpr => evalExpr(argExpr, s));
			if (typeof func == "object" && func.constructor == Func) {
				return func.call(argValues);
			}
			else if (typeof func == "object" && func.constructor == Class) {
				let clazz = func;
				return clazz.instantiate(argValues);
			}
			else {
				throw "Expected function for function call but found other type";
			}
		},
		"object-field": (e, s) => {
			let object = evalExpr(e.data.object, s);
			if (typeof object == "object") {
				return object[e.data.field];
			}
			else {
				throw "Object field access, expected object but found " + (typeof object);
			}
		},
		"if": (e, s) => {
			for (let conditional of e.data.conditionals) {
				if (conditional.condition == undefined) { // else case
					return evalExpr(conditional.body, s);
				}
				else {
					let condition = evalExpr(conditional.condition, s);
					if (condition) {
						return evalExpr(conditional.body, s);
					}
				}
			}
		},
		"assignment": (e, s) => {
			if (e.data.variable.type == "variable") {
				let value = evalExpr(e.data.value, s);
				s[e.data.variable.data] = value;
				return value;
			}
			else if (e.data.variable.type == "object-field") {
				let value = evalExpr(e.data.value, s);
				let object = evalExpr(e.data.variable.data.object, s);
				let field = e.data.variable.data.field;
				object[field] = value;
				return value;
			}
		},
		"joint": (e, s) => {
			let result;
			for (let expr of e.data.exprs) {
				result = evalExpr(expr, s);
			}
			return result;
		},
	};

	/*
		Evaluates an expression and returns the result
		@param expr - Expr - the expr to evaluate
		@param scope - Scope - the scope to evaluate in
		@return any - the result of evaluation	
	*/
	function evalExpr(expr, scope) {
		if (expr.type in EXPR_EVALUATORS) {
			return EXPR_EVALUATORS[expr.type](expr, scope);
		}
		else {
			throw "Invalid expression type: " + expr.type;
		}
	}

	/*
		Evaluates the given source code
		@param s - string - the source
		@return any - the result of evaluation
	*/
	function run(s) {
		let expr = SimpleParser.parse(s);
		let scope = {};
		let result = evalExpr(expr, scope);
		return result;
	}

	return {
		run
	};

})();
