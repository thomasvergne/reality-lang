enum Value {
    Str(String),
    Arr(List[String])
}

enum Configuration {
    Section(List[String], List[*Configuration]),
    KeyValue(String, Value)
}

impl fn (c: List[String]) join(sep: String) -> String {
    let result = "";
    let i = 0u64;
    while i < c.length {
        result = result + c[i];
        if i + 1 < c.length {
            result = result + sep;
        };
        i = i + 1;
    };
    return result;
}

impl fn (c: Configuration) show_prec(prec: i32) -> String {
    if c is Section(let name, let configs) {
        let result = "[" + name.join(".") + "]\n";

        let i = 0u64;
        while i < configs.length {
            let config = configs[i];
            result = result + config->show_prec(prec + 1);

            if i + 1 < configs.length {
                result = result + "\n";
            };

            i = i + 1;
        };

        return result;
    } else if c is KeyValue(let key, let value) {
        let result = key.show_prec(0) + " = " + value.show_prec(prec + 1);
        return result;
    } else {
        return "<invalid configuration>";
    }
}

impl fn (c: List[*Configuration]) get_section(name: List[String]) -> Option[Configuration] {
    let i = 0u64;
    while i < c.length {
        let config = *c[i];
        if config is Section(let sectionName, let sectionConfigs) {
            if sectionName == name {
                return Some(config);
            };
        };
        i = i + 1;
    };

    return None;
}

impl fn (c: List[*Configuration]) get_key_value(key: String) -> Option[Value] {
    let i = 0u64;
    while i < c.length {
        let config = *c[i];
        if config is KeyValue(let k, let v) && k == key {
            return Some(v);
        };
        i = i + 1;
    };

    return None;
}

impl fn (c: String) starts_with(prefix: String) -> bool {
    if prefix.length > c.length {
        return false;
    };

    let i = 0u64;
    while i < prefix.length {
        if c[i] != prefix[i] {
            return false;
        };
        i = i + 1;
    };

    return true;
}

impl fn (c: List[A]) take_while[A](predicate: fn(A) -> bool) -> List[A] {
    let result = List::new::[A]();

    let i = 0u64;
    while i < c.length {
        let value = c[i];
        if predicate(value) {
            List::push(result, value);
        } else {
            break;
        };
        i = i + 1;
    };

    return *result;
}

impl fn (c: List[A]) drop_while[A](predicate: fn(A) -> bool) -> List[A] {
    let result = List::new::[A]();

    let i = 0u64;
    while i < c.length {
        let value = c[i];
        if not(predicate(value)) {
            break;
        };
        i = i + 1;
    };

    while i < c.length {
        let value = c[i];
        List::push(result, value);
        i = i + 1;
    };

    return *result;
}

impl fn (c: String) split(sep: char) -> List[String] {
    let result = List::new::[String]();

    let currentPart = "";
    let i = 0u64;
    while i < c.length {
        let c_char = c[i];
        if c_char == sep {
            List::push(result, currentPart);
            currentPart = "";
        } else {
            currentPart = currentPart + String::new(GC::allocate(c_char));
        };
        i = i + 1;
    };

    List::push(result, currentPart);

    return *result;
}

impl fn (c: List[A]) partition[A](predicate: fn(A) -> bool) -> Tuple[List[A], List[A]] {
    let trueList = List::new::[A]();
    let falseList = List::new::[A]();

    let i = 0u64;
    while i < c.length {
        let value = c[i];
        if predicate(value) {
            List::push(trueList, value);
        } else {
            List::push(falseList, value);
        };
        i = i + 1;
    };

    return Pair(*trueList, *falseList);
}

impl fn 
    (c: List[*Configuration])
    build_tree() -> List[*Configuration]
{
    if c.length == 0u64 {
        return [];
    };

    let first = c[0u64];
    let rest = c.slice(1u64, c.length);

    if (*first) is Section(let name, let _) {
        let sectionConfigs = List::new::[*Configuration]();
        let others = List::new::[*Configuration]();

        let i = 0u64;
        let is_correct = true;

        while i < rest.length {
            let config = rest[i];
            if (*config) is Section(let sectionName, let _) {
                if sectionName.join(".").starts_with(name.join(".")) && is_correct {
                    List::push(sectionConfigs, config);
                    is_correct = true;
                } else {
                    List::push(others, config);
                    is_correct = false;
                };
            } else if is_correct {
                List::push(sectionConfigs, config);
                is_correct = true;
            } else {
                List::push(others, config);
                is_correct = false;
            }
            i = i + 1;
        }
        let sectionConfigs_ = sectionConfigs->build_tree();

        let others_ = others->build_tree();

        let new_section = GC::allocate(Section(name, sectionConfigs_));

        return [new_section].extend(others_);
    }

    let rest_built = rest.build_tree();

    return [first].extend(rest_built);
}
