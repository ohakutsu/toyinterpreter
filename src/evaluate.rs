use crate::ast::{Expression, Infix, Literal, Statement};
use crate::object::Object;
use std::collections::HashMap;

pub struct Evaluator {
    local_variables: HashMap<String, Object>,
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    pub fn new() -> Self {
        let local_variables = HashMap::new();
        Self { local_variables }
    }

    pub fn eval(&mut self, node: Statement) -> Option<Object> {
        self.eval_stmt(&node)
    }

    fn get_lvar(&self, name: &String) -> Object {
        match self.local_variables.get(name) {
            Some(obj) => obj.clone(),
            None => panic!("undefined variable: {}", name),
        }
    }

    fn obj_to_bool(&self, obj: Object) -> bool {
        match obj {
            Object::Int(n) => n != 0,
            Object::Boolean(b) => b,
        }
    }

    fn eval_stmt(&mut self, stmt: &Statement) -> Option<Object> {
        match stmt {
            Statement::Expression(expr) => Some(self.eval_expr(expr)),
            Statement::If { cond, then } => {
                let obj = self.eval_expr(cond);
                if self.obj_to_bool(obj) {
                    self.eval_stmt(then);
                }
                None
            }
            Statement::For {
                init,
                cond,
                inc,
                then,
            } => {
                if let Some(node) = init {
                    self.eval_expr(node);
                }
                loop {
                    let cond = match cond {
                        Some(node) => {
                            let obj = self.eval_expr(node);
                            self.obj_to_bool(obj)
                        }
                        None => true,
                    };
                    if !cond {
                        break;
                    }

                    self.eval_stmt(then);

                    if let Some(node) = inc {
                        self.eval_expr(node);
                    }
                }

                None
            }
            Statement::Print(val) => {
                let obj = self.eval_expr(val);
                println!("{}", obj);
                None
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expression) -> Object {
        match expr {
            Expression::Ident(ident) => self.eval_ident_expr(ident),
            Expression::Literal(literal) => self.eval_literal_expr(literal),
            Expression::Infix { op, lhs, rhs } => self.eval_infix_expr(op, lhs, rhs),
            _ => panic!("unknown node: {:?}", expr),
        }
    }

    fn eval_ident_expr(&self, ident: &String) -> Object {
        self.get_lvar(ident)
    }

    fn eval_literal_expr(&self, literal: &Literal) -> Object {
        match literal {
            Literal::Int(n) => Object::Int(*n),
            _ => panic!("unknown literal: {:?}", literal),
        }
    }

    fn eval_infix_expr(&mut self, op: &Infix, lhs: &Expression, rhs: &Expression) -> Object {
        if let Infix::Assign = op {
            if let Expression::Ident(name) = lhs {
                let obj = self.eval_expr(rhs);
                self.local_variables.insert(name.to_string(), obj.clone());
                return obj;
            }
        };

        let lhs = self.eval_expr(lhs);
        let rhs = self.eval_expr(rhs);

        match lhs {
            Object::Int(left) => match rhs {
                Object::Int(right) => self.eval_infix_int_expr(op, left, right),
                _ => panic!("mismatch type: {} {:?} {}", lhs, op, rhs),
            },
            _ => panic!("unknown operation: {} {:?} {}", lhs, op, rhs),
        }
    }

    fn eval_infix_int_expr(&self, op: &Infix, lhs: i32, rhs: i32) -> Object {
        match op {
            Infix::Plus => Object::Int(lhs + rhs),
            Infix::Minus => Object::Int(lhs - rhs),
            Infix::Multiply => Object::Int(lhs * rhs),
            Infix::Divide => {
                if rhs == 0 {
                    panic!("Zero division error");
                }
                Object::Int(lhs / rhs)
            }
            Infix::Equal => Object::Boolean(lhs == rhs),
            Infix::NotEqual => Object::Boolean(lhs != rhs),
            Infix::GreaterThan => Object::Boolean(lhs > rhs),
            Infix::GreaterThanEqual => Object::Boolean(lhs >= rhs),
            Infix::LessThan => Object::Boolean(lhs < rhs),
            Infix::LessThanEqual => Object::Boolean(lhs <= rhs),
            _ => panic!("unknown operation: {} {:?} {}", lhs, op, rhs),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluate::*;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parse::Parser;

    fn assert_eval(input: &str, expect_values: &[Option<Object>]) {
        let tokens = Lexer::new(input).collect();
        let mut parser = Parser::new(tokens);
        let nodes = parser.parse();
        let mut evaluator = Evaluator::new();

        for (node, expect) in nodes.into_iter().zip(expect_values) {
            let return_val = evaluator.eval(node);
            assert_eq!(return_val, *expect);
        }
    }

    #[test]
    fn test_evaluator_assign() {
        assert_eval(
            "abc = 1;\
             def = 2;\
             abc + def;",
            &[
                Some(Object::Int(1)),
                Some(Object::Int(2)),
                Some(Object::Int(3)),
            ],
        );

        assert_eval(
            "abc = 1 + 2;\
             def = abc + 3;\
             def;",
            &[
                Some(Object::Int(3)),
                Some(Object::Int(6)),
                Some(Object::Int(6)),
            ],
        );
    }

    #[test]
    fn test_evaluator_infix() {
        assert_eval(
            "a = 1 + 2 * 3;\
             b = 4 / 2 - 1;",
            &[Some(Object::Int(7)), Some(Object::Int(1))],
        );
    }

    #[test]
    fn test_evaluator_equality_relational() {
        assert_eval(
            "abc = 1;\
             def = 2;\
             ghi = 0;\
             abc > def;\
             abc < def;",
            &[
                Some(Object::Int(1)),
                Some(Object::Int(2)),
                Some(Object::Int(0)),
                Some(Object::Boolean(false)),
                Some(Object::Boolean(true)),
            ],
        );

        assert_eval(
            "abc = 2;\
             def = 2;\
             ghi = 0;\
             abc >= def;\
             abc <= def;",
            &[
                Some(Object::Int(2)),
                Some(Object::Int(2)),
                Some(Object::Int(0)),
                Some(Object::Boolean(true)),
                Some(Object::Boolean(true)),
            ],
        );
    }

    #[test]
    fn test_evaluator_if() {
        assert_eval(
            "i = 1;\
             if (i <= 10) i = i + 10;\
             i;",
            &[Some(Object::Int(1)), None, Some(Object::Int(11))],
        );

        assert_eval(
            "i = 2;\
             if (i > 3) i = 0;\
             i;",
            &[Some(Object::Int(2)), None, Some(Object::Int(2))],
        );
    }

    #[test]
    fn test_evaluator_print() {
        assert_eval(
            "i = 2;\
             print i;",
            &[Some(Object::Int(2)), None],
        );
    }

    #[test]
    fn test_evaluator_for() {
        assert_eval(
            "i = 0;\
             for (i = 0; i < 3; i = i + 1) print i;\
             i;",
            &[Some(Object::Int(0)), None, Some(Object::Int(3))],
        );

        assert_eval(
            "i = 0;\
             for (; i < 3;) i = i + 1;\
             i;",
            &[Some(Object::Int(0)), None, Some(Object::Int(3))],
        );
    }
}
