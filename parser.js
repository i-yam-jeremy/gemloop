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
			"if", "elif", "else"
		];

		/* The regular expressions for matching tokens */
		const TOKEN_REGEXES = {
			"number": /^(?:[0-9]*\.[0-9]+)|^(?:[0-9]+)/,
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
			"whitespace": /^\s+/
		};

		/* The token data container class */
		class Token {
			/*
				Creates a token with the given data
				@param type - string - the type of the token
				@param value - string - the string value of the token
			*/
			constructor(type, value) {
				this.type = type;
				this.value = value;
			}
		}

		/* The stream of tokens */
		class TokenStream {

			/*
				Fields:

				tokens - Token[] - the source array of tokens
				position - natural_number - the current position in the token stream
				savedStates - natural_number[] - the saved position states, used to restore to a previous position state
			*/

			/* 
				Creates a token stream from the given array of tokens
				@param tokens - Token[] - the sourcce array of tokens
			*/
			constructor(tokens) {
				this.tokens = tokens;
				this.position = 0;
				this.savedStates = [];
				
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
					preprocessedTokens.push(Token("comment", body));
					return true;
				}
				return false;
			}

			/*
				Preprocess a keyword if one is available at the current position
				@param processedTokens - Token[] - the output array of already processed tokens
				@return boolean - true iff a keyword was found and added to processedTokens, otherwise false
			*/
			__preprocessKeyword(preprocessedTokens) {
				let token = this.tokens[this.position];
				if (token.type == "identifier" && KEYWORDS.indexOf(token.value) > -1) {
					preprocessedTokens.push(new Token("keyword", token.value));
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
		}

		/*
			Converts the input string into a stream of tokens
			@param s - string - the input string
			@return TokenStream - the stream of tokens
		*/
		function lex(s) {
			let tokens = [];
			while (s.length > 0) {
				let foundToken = false;
				for (let tokenType in TOKEN_REGEXES) {
					let result = TOKEN_REGEXES[tokenType].exec(s);
					if (result && result[0] != null) {
						let value = result[0];
						tokens.push(new Token(tokenType, value));
						foundToken = true;
						s = s.substring(value.length);
						break;
					}
				}
				if (!foundToken) {
					throw "Unidentitifed token: " + s;
				}
			}
			return new TokenStream(tokens);
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
					throw "Expected ')' but found '" + endToken.type + "'";
				}
			}
			else {
				tokens.restore();
				return false;
			}
		}

		/*
			Attempts to parse an integer literal expression
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an integer literal expr, otherwise false
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
			let exprParsers = [parentheses, numberLiteral, stringLiteral, variableName];
			for (let parser of exprParsers) {
				let expr = parser(tokens);
				if (expr) {
					return expr;
				}
			}
			throw "Expected atomic expression (parentheses, integer literal, or variable name) but none found.";
		}

		/*
			Attempts to parse a lambda expression
			A Lambda Expression is one of:
				Atomic Expression,
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
						return atom(tokens);
					}
				}
			}
			else {
				tokens.restore();
				return atom(tokens);
			}

			if (!(tokens.next().type == "=" && tokens.next().type == ">")) { // if "=>" not found
				tokens.restore();
				return atom(tokens);
			}

			if (tokens.next().type == "{") {
				let body = parseExpr(tokens);
				if (!body) {
					throw "No lambda expression body found";
				}
				if (tokens.next().type == "}") {
					tokens.clearSave();
					return new Expr("lambda", {params: paramNames, body: body});
				}
				else {
					tokens.restore();
					return atom(tokens);
				}
			}
			else {
				tokens.restore();
				return atom(tokens);
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
					parentClassExpr = parseExpr(tokens);
					if (!parentClassExpr) {
						tokens.restore();
						return lambda(tokens);
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
					let methodNameToken = tokens.next();
					if (methodNameToken.type != "identifier") {
						tokens.restore();
						return lambda(tokens);
					}
					let methodName = methodNameToken.value;
					let method = lambda(tokens); // parse lambda expression
					if (method.type != "lambda") { // if next is not a lambda expression, restore and fall-through since it is not a class definition or there is an error
						tokens.restore();
						return lambda(tokens);
					}
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
							let arg = parseExpr(tokens);
							if (!arg) {
								throw "Expected argument expression but none found";
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
								throw "Expected ',' or ')' but found '" + commaToken.type + "'";
							}
						}
						expr = new Expr("function-call", {func: expr, args: args});
					}
				}
				else if (type == ".") { // parse object field expr
					let fieldName = variableName(tokens);
					if (!fieldName) {
						throw "Expected field name but none found";
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
				expr = new Expr(op, {left: expr, right: rhs});
			}
			tokens.restore(); // undo the last next token request because it was not an operator
			return expr;
		}

		/*
			Attempts to parse an if expression
			An If Expression is one of:
				Addition or Subtraction Expression,
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
				}

				if (tokens.next().type == "{") {
					let body = parseExpr(tokens);
					if (!body) {
						throw "Expected if-expression body but found none";
					}
					if (tokens.next().type == "}") {
						conditionals.push({condition: conditionExpr, body: body});
					}
					else {
						tokens.restore();
						return addSub(tokens);
					}
				}
				else {
					tokens.restore();
					return addSub(tokens);
				}
				
			}
			tokens.restore(); // restore last unmatched keyword attempt
			if (conditionals.length > 0) {
				return new Expr("if", {conditionals: conditionals});
			}
			else {
				tokens.restore();
				return addSub(tokens);
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
			if (lhs.type != "variable") { // TODO add object field assignment
				return lhs;
			}
			if (tokens.next().type != "=") {
				console.log(tokens);
				tokens.restore();
				return ifElse(tokens);
			}
			let rhs = assignment(tokens); // right-hand side
			if (rhs) { 
				tokens.clearSave();
				return new Expr("assignment", {variable: lhs, value: rhs});
			}
			else {
				tokens.restore();
				return ifElse(tokens);
			}
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
			let expr = assignment(tokens);
			while (tokens.save(), tokens.next().type == ",") { // save in case token is not ","
				tokens.clearSave(); // the token was "," so no need to restore
				let nextExpr = assignment(tokens);
				if (!nextExpr) {
					throw "Expected another expression but found none";
				}
				expr = new Expr("joint", {left: expr, right: nextExpr});
			}
			tokens.restore(); // restore since last token was not ","
			return expr;
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

var result = SimpleParser.parse(`
anInt = 108,
aFloat = 109.34
`);

console.log(result);
