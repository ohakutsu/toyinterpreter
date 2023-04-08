use crate::ast::{Expression, Infix, Literal, Statement};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Object {
    Num(i32),
    Boolean(bool),
}

pub struct Evaluator {
    local_variables: HashMap<String, i32>,
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

    fn get_lvar(&self, name: &String) -> i32 {
        match self.local_variables.get(name) {
            Some(n) => *n,
            None => panic!("undefined variable: {}", name),
        }
    }

    fn expect_num_obj(&self, obj: &Object) -> i32 {
        match obj {
            Object::Num(n) => *n,
            _ => panic!("expect num object, but {:?}", obj),
        }
    }

    fn obj_to_bool(&self, obj: &Object) -> bool {
        match obj {
            Object::Num(n) => *n != 0,
            Object::Boolean(b) => *b,
        }
    }

    fn eval_stmt(&mut self, node: &Statement) -> Option<Object> {
        match node {
            Statement::Expression(expr) => Some(self.eval_expr(expr)),
            Statement::If { cond, then } => {
                let obj = self.eval_expr(cond);
                if self.obj_to_bool(&obj) {
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
                            self.obj_to_bool(&obj)
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
                let n = self.expect_num_obj(&obj);
                println!("{}", n);
                None
            }
        }
    }

    fn eval_expr(&mut self, node: &Expression) -> Object {
        match node {
            Expression::Ident(ident) => self.eval_ident_expr(ident),
            Expression::Literal(literal) => self.eval_literal_expr(literal),
            Expression::Infix { op, lhs, rhs } => self.eval_infix_expr(op, lhs, rhs),
            _ => panic!("unknown node: {:?}", node),
        }
    }

    fn eval_ident_expr(&self, ident: &String) -> Object {
        let n = self.get_lvar(ident);
        Object::Num(n)
    }

    fn eval_literal_expr(&self, literal: &Literal) -> Object {
        match literal {
            Literal::Int(n) => Object::Num(*n),
            _ => panic!("unknown literal: {:?}", literal),
        }
    }

    fn eval_infix_expr(&mut self, op: &Infix, lhs: &Expression, rhs: &Expression) -> Object {
        match op {
            Infix::Plus => {
                let lhs_obj = self.eval_expr(lhs);
                let rhs_obj = self.eval_expr(rhs);
                let lhs = self.expect_num_obj(&lhs_obj);
                let rhs = self.expect_num_obj(&rhs_obj);
                Object::Num(lhs + rhs)
            }
            Infix::Minus => {
                let lhs_obj = self.eval_expr(lhs);
                let rhs_obj = self.eval_expr(rhs);
                let lhs = self.expect_num_obj(&lhs_obj);
                let rhs = self.expect_num_obj(&rhs_obj);
                Object::Num(lhs - rhs)
            }
            Infix::Assign => {
                if let Expression::Ident(name) = lhs {
                    let obj = self.eval_expr(rhs);
                    let n = self.expect_num_obj(&obj);
                    self.local_variables.insert(name.to_string(), n);
                    Object::Num(n)
                } else {
                    panic!("syntax error")
                }
            }
            Infix::Equal => {
                let lhs_obj = self.eval_expr(lhs);
                let rhs_obj = self.eval_expr(rhs);
                let lhs = self.expect_num_obj(&lhs_obj);
                let rhs = self.expect_num_obj(&rhs_obj);
                Object::Boolean(lhs == rhs)
            }
            Infix::NotEqual => {
                let lhs_obj = self.eval_expr(lhs);
                let rhs_obj = self.eval_expr(rhs);
                let lhs = self.expect_num_obj(&lhs_obj);
                let rhs = self.expect_num_obj(&rhs_obj);
                Object::Boolean(lhs != rhs)
            }
            Infix::GreaterThan => {
                let lhs_obj = self.eval_expr(lhs);
                let rhs_obj = self.eval_expr(rhs);
                let lhs = self.expect_num_obj(&lhs_obj);
                let rhs = self.expect_num_obj(&rhs_obj);
                Object::Boolean(lhs > rhs)
            }
            Infix::GreaterThanEqual => {
                let lhs_obj = self.eval_expr(lhs);
                let rhs_obj = self.eval_expr(rhs);
                let lhs = self.expect_num_obj(&lhs_obj);
                let rhs = self.expect_num_obj(&rhs_obj);
                Object::Boolean(lhs >= rhs)
            }
            Infix::LessThan => {
                let lhs_obj = self.eval_expr(lhs);
                let rhs_obj = self.eval_expr(rhs);
                let lhs = self.expect_num_obj(&lhs_obj);
                let rhs = self.expect_num_obj(&rhs_obj);
                Object::Boolean(lhs < rhs)
            }
            Infix::LessThanEqual => {
                let lhs_obj = self.eval_expr(lhs);
                let rhs_obj = self.eval_expr(rhs);
                let lhs = self.expect_num_obj(&lhs_obj);
                let rhs = self.expect_num_obj(&rhs_obj);
                Object::Boolean(lhs <= rhs)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluate::*;
    use crate::lexer::Lexer;
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
                Some(Object::Num(1)),
                Some(Object::Num(2)),
                Some(Object::Num(3)),
            ],
        );

        assert_eval(
            "abc = 1 + 2;\
             def = abc + 3;\
             def;",
            &[
                Some(Object::Num(3)),
                Some(Object::Num(6)),
                Some(Object::Num(6)),
            ],
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
                Some(Object::Num(1)),
                Some(Object::Num(2)),
                Some(Object::Num(0)),
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
                Some(Object::Num(2)),
                Some(Object::Num(2)),
                Some(Object::Num(0)),
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
            &[Some(Object::Num(1)), None, Some(Object::Num(11))],
        );

        assert_eval(
            "i = 2;\
             if (i > 3) i = 0;\
             i;",
            &[Some(Object::Num(2)), None, Some(Object::Num(2))],
        );
    }

    #[test]
    fn test_evaluator_print() {
        assert_eval(
            "i = 2;\
             print i;",
            &[Some(Object::Num(2)), None],
        );
    }

    #[test]
    fn test_evaluator_for() {
        assert_eval(
            "i = 0;\
             for (i = 0; i < 3; i = i + 1) print i;\
             i;",
            &[Some(Object::Num(0)), None, Some(Object::Num(3))],
        );

        assert_eval(
            "i = 0;\
             for (; i < 3;) i = i + 1;\
             i;",
            &[Some(Object::Num(0)), None, Some(Object::Num(3))],
        );
    }
}
