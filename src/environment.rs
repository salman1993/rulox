use std::{cell::RefCell, collections::HashMap};

use crate::evaluate::LoxValue;

#[derive(Debug)]
pub struct Environment<'a> {
    // Rust allows only 1 mutable reference but each child env needs to be be mutate the parent env (for assign)
    // we need RefCell here for interior mutability - allows us to move borrow checking to runtime
    bindings: RefCell<HashMap<String, LoxValue>>,
    // The parent reference must live at least as long as 'a (child environment), preventing dangling references.
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        }
    }

    pub fn new_child(parent: &'a Environment<'a>) -> Self {
        // The child borrows the parent, i.e. the child cannot outlive the parent
        // If the parent is dropped, the child’s parent reference would become invalid
        Environment {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(parent),
        }
    }

    pub fn get_global_env(&self) -> &Environment<'a> {
        let mut current = self;
        while let Some(parent) = current.parent {
            current = parent;
        }
        current
    }

    pub fn get_parent(&self) -> Option<&'a Environment<'a>> {
        self.parent
    }

    pub fn var(&self, name: &str, value: LoxValue) {
        let mut bindings = self.bindings.borrow_mut();
        bindings.insert(name.to_string(), value);
    }

    pub fn assign(&self, name: &str, value: LoxValue) -> Result<(), String> {
        let mut bindings = self.bindings.borrow_mut();
        if bindings.get(name).is_some() {
            bindings.insert(name.to_string(), value);
            return Ok(());
        } else if let Some(parent) = self.get_parent() {
            return parent.assign(name, value);
        }

        Err(format!(
            "Error: cannot assign because no binding exists for {name}"
        ))
    }

    pub fn lookup(&self, name: &str) -> Result<LoxValue, String> {
        let bindings = self.bindings.borrow();
        if let Some(val) = bindings.get(name) {
            return Ok(val.clone()); // TODO: is it bad to clone the value here??
        } else if let Some(parent) = self.get_parent() {
            let value = parent.lookup(name);
            return value;
        }

        Err(format!(
            "Error: cannot lookup because no binding exists for {name}"
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evaluate::LoxValue::*;

    #[test]
    fn test_new_env_is_empty() {
        let env = Environment::new();
        assert!(env.bindings.borrow().is_empty());
    }

    #[test]
    fn test_env_var_adds_binding() {
        let env = Environment::new();
        env.var("x", VNumber(42.0));
        assert_eq!(env.lookup("x"), Ok(VNumber(42.0)));
    }

    #[test]
    fn test_env_lookup_nonexistent_variable() {
        let env = Environment::new();
        assert!(env.lookup("y").is_err());
    }

    #[test]
    fn test_env_assign() {
        let env = Environment::new();
        env.var("x", VNumber(42.0));
        assert!(env.assign("x", VNumber(99.0)).is_ok());
        assert_eq!(env.lookup("x"), Ok(VNumber(99.0)));
    }

    #[test]
    fn test_env_assign_nonexistent_variable() {
        let env = Environment::new();
        assert!(env.assign("y", VNumber(99.0)).is_err());
    }

    #[test]
    fn test_env_assign_in_parent() {
        let env = Environment::new();
        env.var("x", VNumber(10.0));
        assert!(env.assign("x", VNumber(22.0)).is_ok());
        assert_eq!(env.lookup("x"), Ok(VNumber(22.0)));
        let child = Environment::new_child(&env);
        assert!(child.assign("x", VNumber(95.0)).is_ok());
    }

    #[test]
    fn test_env_assign_nested() {
        let env = Environment::new();
        env.var("x", VNumber(10.0));
        let child = Environment::new_child(&env);
        let grandchild = Environment::new_child(&child);
        let _ = grandchild.assign("x", VNumber(-25.3));
        assert_eq!(grandchild.lookup("x"), Ok(VNumber(-25.3)));
    }

    #[test]
    fn test_env_lookup_in_parent() {
        let env = Environment::new();
        env.var("x", VNumber(10.0));
        let child = Environment::new_child(&env);
        assert_eq!(child.lookup("x"), Ok(VNumber(10.0)));
    }

    #[test]
    fn test_env_lookup_nested() {
        let env = Environment::new();
        env.var("x", VNumber(10.0));
        let child = Environment::new_child(&env);
        let grandchild = Environment::new_child(&child);
        assert_eq!(grandchild.lookup("x"), Ok(VNumber(10.0)));
    }

    #[test]
    fn test_env_shadow_variable_in_parent() {
        let env = Environment::new();
        env.var("x", VNumber(10.0));
        let child = Environment::new_child(&env);
        child.var("x", VNumber(66.0));
        assert_eq!(child.lookup("x"), Ok(VNumber(66.0)));
    }
}
