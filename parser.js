const SimpleParser = (() => {

	const Lexer = (() => {

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
			"whitespace": /^\s+/
		};

		class Token {
			constructor(type, value) {
				this.type = type;
				this.value = value;
			}
		}

		class TokenList {

			constructor(tokens) {
				this.tokens = tokens;
				this.position = 0;
				this.savedStates = [];
			}

			next() {
				if (this.position >= this.tokens.length) {
					return new Token("EOF", "");
				}
				else {
					return this.tokens[this.position++];
				}
			}

			save() {
				this.savedStates.push(this.position);
			}

			restore() {
				this.position = this.savedStates.pop();
			}
		}

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
			return new TokenList(tokens);
		}

		return {
			lex
		};

	})();

	const ExprParser = (() => {

		class Expr {
			constructor(type, data) {
				this.type = type;
				this.data = data;
			}
		}

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

		function mulDivMod(tokens) {
			let expr = atom(tokens);
			let op;
			tokens.save(); // save before the first operator token request in case it is not and operator
			while (["*", "/", "%"].indexOf((op = tokens.next().type)) > -1) {
				let rhs = atom(tokens); // right-hand side
				expr = new Expr(op, {left: expr, right: rhs});
				tokens.save(); // in case next token is not one of '*', '/', '%'
			}
			tokens.restore(); // undo the last next token request because it was not an operator
			return expr;
		}

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

		function parseExpr(tokens) {
			return addSub(tokens);
		}

		return {
			parseExpr
		};

	})();

	function parse(s) {
		const tokens = Lexer.lex(s);
		return ExprParser.parseExpr(tokens);
	}

	return {
		parse
	};

})();

var result = SimpleParser.parse(`

10 + x*4

`);

console.log(result);
