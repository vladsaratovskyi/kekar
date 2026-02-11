#[cfg(test)]
mod tests {
    use kekar::{generator::JsGenerator, lexer::Lexer, parser::Parser};

    #[test]
    fn compile_person_program_to_js() {
        let source = r#"
import System from "../src/system.kek";

class Person {
    var name: String;
    var age: Num;
    var items: String[];

    fun init() {
        this.name = "Name";
        this.age = 42;
        this.items = ["item1", "item2"];
    }

    fun find_item(String to_find): Num {
        for item, index in this.items {
            if item == to_find {
                return index;
            } else {
                return -1;
            }
        }
    }
}
"#;

        let mut lexer = Lexer::from_source(source);
        let tokens = lexer.lex_file();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();

        let output = JsGenerator::new().generate(&ast);

        assert!(output.contains("import System from \"../src/system.kek\";"));
        assert!(output.contains("class Person {"));
        assert!(output.contains("name;"));
        assert!(output.contains("items;"));
        assert!(output.contains("init() {"));
        assert!(output.contains("this.name = \"Name\";"));
        assert!(output.contains("for (const [index, item] of this.items.entries()) {"));
        assert!(output.contains("if ((item == to_find)) {"));
        assert!(output.contains("return (-1);"));
    }
}
