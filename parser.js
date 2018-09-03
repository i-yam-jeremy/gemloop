const SimpleParser = (() => {

	const Lexer = (() => {

		const TOKEN_REGEXES = {
			"integer": /^[0-9]+/,
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
				return this.tokens[this.position++];
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
			return new TokenList(tokens);
		}

		return {
			lex
		};

	})();

	const ExprParser = (() => {

		function atomExpr(tokens) {

		}

		function parseExpr(tokens) {

		}

		return {
			parseExpr
		};

	})();

	function parse(s) {
		const tokens = Lexer.lex(s);
		console.log(tokens.tokens);
		return ExprParser.parseExpr(tokens);
	}

	return {
		parse
	};

})();

SimpleParser.parse(`

10

`);
