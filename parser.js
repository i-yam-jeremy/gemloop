/*

	A library for parsing a simple language

*/
const SimpleParser = (() => {

	/*
		The lexer/tokenizer for the language
		Converts an input string into a stream of tokens
	*/
	const Lexer = (() => {

		/* The regular expressions for matching tokens */
		const TOKEN_REGEXES = {
			"integer": /^[0-9]+/,
			"identifier": /^[A-Za-z_][A-Za-z_0-9]*/,
			"(": /^\(/,
			")": /^\)/,
			"[": /^\[/,
			"]": /^\]/,
			"+": /^\+/,
			"-": /^\-/,
			"/": /^\//,
			"*": /^\*/,
			"%": /^\%/,
			"<": /^\</,
			">": /^\>/,
			".": /^\./,
			",": /^\,/,
			"!": /^\!/,
			"=": /^\=/,
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
			}

			/*
				Gets the next token in the token stream
				@return Token - the next token in the stream, or a Token with type "EOF" if reached the end of the stream
			*/
			next() {
				if (this.position >= this.tokens.length) {
					return new Token("EOF", "");
				}
				else {
					return this.tokens[this.position++];
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
						if (tokenType != "whitespace") {
							tokens.push(new Token(tokenType, value));
						}
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
		function integerLiteral(tokens) {
			tokens.save();
			let nextToken = tokens.next();
			if (nextToken.type == "integer") {
				return new Expr("literal", parseInt(nextToken.value));
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
			let exprParsers = [parentheses, integerLiteral, variableName];
			for (let parser of exprParsers) {
				let expr = parser(tokens);
				if (expr) {
					return expr;
				}
			}
			throw "Expected atomic expression (parentheses, integer literal, or variable name) but none found.";
		}

		/*
			Attempts to parse a function call or object field expression
			A Function Call or Object Field Expression is one of:
				Atomic Expression,
				{
					func: Atomic Expression,
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
			let expr = atom(tokens);
			if (!expr) {
				tokens.restore();
				return false;
			}

			let type;
			while (tokens.save(), ["(", "."].indexOf((type = tokens.next().type)) > -1) { // save for when next token is not '(', and loop so multiple function calls can be strung together like f()(x)(y)
				if (type == "(") { // parse function call expr
					let args = [];
					while (tokens.save(), tokens.next().type != ")") { // save so non ')' token can be restored and used in parsing argument expression
						tokens.restore(); // undo last token request since the token was not an ')'
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
				else if (type == ".") { // parse object field expr
					let fieldName = variableName(tokens);
					if (!fieldName) {
						throw "Expected field name but none found";
					}
					expr = new Expr("object-field", {object: expr, field: fieldName.data});
				}
			}
			tokens.restore(); // undo last request of token because it was not a '('
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
			tokens.save(); // save before the first operator token request in case it is not and operator
			while (["*", "/", "%"].indexOf((op = tokens.next().type)) > -1) {
				let rhs = functionCallOrObjectField(tokens); // right-hand side
				expr = new Expr(op, {left: expr, right: rhs});
				tokens.save(); // in case next token is not one of '*', '/', '%'
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
			tokens.save(); // save before the first operator token request in case it is not and operator
			while (["+", "-"].indexOf((op = tokens.next().type)) > -1) {
				let rhs = mulDivMod(tokens); // right-hand side
				expr = new Expr(op, {left: expr, right: rhs});
				tokens.save(); // in case next token is not one of '+', '-'
			}
			tokens.restore(); // undo the last next token request because it was not an operator
			return expr;
		}

		/*
			Attempts to parse an expr from the given token stream
			@param tokens - TokenStream - the token stream
			@return Expr? - the expr if the token stream matches an expr, otherwise false
		*/
		function parseExpr(tokens) {
			return addSub(tokens);
		}

		return {
			parse: parseExpr
		};

	})();

	/*
		Parses statements from the token stream
	*/
	const StatementParser = (() => {

		/* The statement data container class */
		class Statement {
			/*
				Creates a statement from the given data
				@param type - string - the statement type
				@param data - any - the data associated with this statement
			*/
			constructor(type, data) {
				this.type = type;
				this.data = data;
			}
		}

		/*
			The functions for parsing different statement types 
			Signature: (tokens) -> Statement?
			Each function returns the statement if it matches the given statement type, otherwise false
		*/
		const STATEMENT_PARSERS = [
			assignment,
			expr
		];

		/*
			Attempts to parse an assignment statement
			An Assignment Statement is:
				<Variable Name Expression> = <Expression>
			@param tokens - TokenStream - the token stream
			@return Statement? - the statement if it matches an assigment statement, otherwise false
		*/
		function assignment(tokens) {
			tokens.save();
			let lhs = ExprParser.parse(tokens); // left-hand side
			let equalsToken = tokens.next();
			let rhs = ExprParser.parse(tokens); // right-hand side
			if (lhs.type == "variable" && equalsToken.type == "=" && rhs) {
				return new Statement("assignment", {variable: lhs.data, value: rhs});
			}
			else {
				tokens.restore();
				return false;
			}
		}

		/*
			Attempts to parse an expression statement
			An Expression Statement is:
				<Expression>
			It is just a single expression as a statement. This is can be used for any
				expression, but is most often used for function call expressions
				where the return value is ignored,
			@param tokens - TokenStream - the token stream
			@return Statement? - the statement if it matches an assigment statement, otherwise false
		*/
		function expr(tokens) {
			tokens.save();
			let expr = ExprParser.parse(tokens);
			if (expr) {
				return new Statement("expr", expr);
			}
			else {
				tokens.restore();
				return false;
			}
		}

		/*
			Attempts to parse a statement of any type
			@param tokens - TokenStream - the token stream
			@return Statement? - the statement if it matches any statement type, otherwise false
		*/
		function parseStatement(tokens) {
			for (let parser of STATEMENT_PARSERS) {
				let statement = parser(tokens);
				if (statement) {
					return statement;
				}
			}
			return false;
		}

		return {
			parse: parseStatement
		};

	})();

	/*
		Parse an input string
		@param s - string - the input source string
		@return Statement[] - the statements described by the source
	*/
	function parse(s) {
		const tokens = Lexer.lex(s);
		return StatementParser.parse(tokens);
	}

	return {
		parse
	};


})();

var result = SimpleParser.parse(`

x = 10 + g(x, y)(z)*4 + window.getBounds(100, 200).width

`);

console.log(result);
