use untwine;

#[derive(Debug, Clone)]
enum Lambda {
    Variable(String),
    Function(String, Box<Lambda>),
    Application(Box<Lambda>, Box<Lambda>),
}

untwine::parser! {
    whitespace = #{char::is_ascii_whitespace}*;
    variable: var=<('a'-'z' | 'A'-'Z' | '0'-'9' | "_" | "$")+> -> String { var.to_string() }
    function: ("lambda" | "\\") whitespace _name=variable (":" | ".") whitespace body=lambda -> Lambda {
      Lambda::Function(_name, Box::new(body))
    }
    application: "(" whitespace left=lambda whitespace right=lambda whitespace ")" -> Lambda { Lambda::Application(Box::new(left), Box::new(right)) }
    _variable: var=variable -> Lambda { Lambda::Variable(var.to_string()) }
    pub lambda: body=(function | _variable | application) -> Lambda { body }
}

#[allow(unused)]
fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn pretty_print(input: &Lambda) -> String {
    match input {
        Lambda::Variable(var) => var.clone(),
        Lambda::Function(var, body) => format!("lambda {}: {}", var, pretty_print(&body)),
        Lambda::Application(left, right) => {
            format!("({} {})", pretty_print(left), pretty_print(right))
        }
    }
}

fn native_dump(input: &Lambda) -> Lambda {
    println!("{}", pretty_print(&evaluate(input)));
    input.clone()
}

fn __native_print(input: &Lambda, depths: Vec<String>) -> String {
    match input {
        Lambda::Variable(var) => {
            if depths.contains(&var) {
                let index = depths
                    .iter()
                    .position(|r| r.clone() == var.clone())
                    .unwrap();
                let formatted = format!("${}", index);

                return formatted;
            } else {
                return var.clone();
            }
        }
        Lambda::Application(left, right) => {
            return format!(
                "({} {})",
                __native_print(&*left, depths.clone()),
                __native_print(&*right, depths)
            );
        }
        Lambda::Function(var, body) => {
            let mut new_depths = depths.clone();
            new_depths.push(var.clone());
            return format!(
                "\\${}. {}",
                depths.len(),
                __native_print(&*body, new_depths)
            );
        }
    }
}

fn native_print(input: &Lambda) -> Lambda {
    println!("{}", __native_print(&evaluate(input), Vec::new()));
    return input.clone();
}

fn native_self(input: &Lambda) -> Lambda {
    input.clone()
}

fn native_nil(input: &Lambda) -> Lambda {
    evaluate(input);
    return Lambda::Variable("nil".to_string());
}

fn native_number(input: &Lambda) -> Lambda {
    match input {
        Lambda::Variable(var) => {
            let number = var.parse::<u32>().unwrap_or(0);
            let mut expr = Lambda::Variable("x".to_string());

            for _ in 0..number {
                expr = Lambda::Application(
                    Box::new(Lambda::Variable("f".to_string())),
                    Box::new(expr),
                );
            }

            return Lambda::Function(
                "f".to_string(),
                Box::new(Lambda::Function("x".to_string(), Box::new(expr))),
            );
        }
        Lambda::Function(_, _) => input.clone(),
        Lambda::Application(_, _) => input.clone(),
    }
}

fn native_resolver(key: String) -> Box<dyn Fn(&Lambda) -> Lambda> {
    if key == "dump" {
        return Box::new(native_dump);
    } else if key == "nil" {
        return Box::new(native_nil);
    } else if key == "print" {
        return Box::new(native_print);
    } else if key == "number" {
        return Box::new(native_number);
    }
    //
    else {
        return Box::new(native_self);
    }
}

fn resolve(left: &Lambda, right: &Lambda) -> Lambda {
    match left {
        Lambda::Variable(key) => native_resolver(key.clone())(right),
        Lambda::Function(_, _) => {
            Lambda::Application(Box::new(evaluate(left)), Box::new(right.clone()))
        }
        Lambda::Application(_, _) => {
            Lambda::Application(Box::new(left.clone()), Box::new(right.clone()))
        }
    }
}

fn reduce(input: &Lambda, find: String, replace: &Lambda) -> Lambda {
    match input {
        Lambda::Variable(var) => {
            if *var == find {
                replace.clone()
            } else {
                input.clone()
            }
        }
        Lambda::Application(_left, _right) => Lambda::Application(
            Box::new(reduce(_left, find.clone(), replace)),
            Box::new(reduce(_left, find.clone(), replace)),
        ),
        Lambda::Function(var, body) => {
            let mut new_body: Lambda;
            let new_name = "_".to_owned() + &find;

            if *var == find {
                // alpha reduction
                new_body = reduce(body, find.clone(), &Lambda::Variable(new_name.clone()));
                new_body = reduce(&new_body, find, replace);
            } else {
                new_body = reduce(body, find, replace);
            }

            Lambda::Function(new_name, Box::new(new_body))
        }
    }
}

fn __evaluate(input: &Lambda) -> Lambda {
    match input {
        Lambda::Variable(_) => input.clone(),
        Lambda::Function(_, _) => input.clone(),
        Lambda::Application(left, right) => {
            let lambda = *left.clone();
            match lambda {
                Lambda::Variable(_) => __evaluate(&resolve(&*left.clone(), right)),
                Lambda::Function(var, body) => reduce(&*body, var.to_string(), right), // TODO: Reduction
                Lambda::Application(_, _) => {
                    Lambda::Application(Box::new(__evaluate(left)), right.clone())
                }
            }
        }
    }
}

fn evaluate(input: &Lambda) -> Lambda {
    let mut x = __evaluate(&input);
    let mut next = __evaluate(&x);

    while __native_print(&next, Vec::new()) != __native_print(&x, Vec::new()) {
        x = next.clone();
        next = __evaluate(&next);
    }

    return next;
}

fn main() {
    let parsed = untwine::parse_pretty(lambda, "(print (number 2))");
    match parsed {
        Ok(v) => {
            evaluate(&v);
        }
        Err(e) => println!("{}", e),
    }
}
