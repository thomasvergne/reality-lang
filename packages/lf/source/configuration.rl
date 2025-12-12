import std.list;

enum Value {
    Str(String),
    Arr(List<String>)
}

enum Configuration {
    Section(List<String>, List<*Configuration>),
    KeyValue(String, Value)
}

impl fn (v: Value) show_prec(prec: int) -> String {
    if v is Str(let s) {
        return "\"" + s + "\"";
    } else if v is Arr(let arr) {
        let result = "[";

        let i = 0;
        while i < arr.length {
            let value = arr[i];
            result = result + "\"" + value + "\"";

            if i + 1 < arr.length {
                result = result + ", ";
            };

            i = i + 1;
        };

        result = result + "]";
        return result;
    } else {
        return "<invalid value>";
    }
}

impl fn (c: List<String>) join(sep: String) -> String {
    let result = "";
    let i = 0;
    while i < c.length {
        result = result + c[i];
        if i + 1 < c.length {
            result = result + sep;
        };
        i = i + 1;
    };
    return result;
}

impl fn (c: Configuration) show_prec(prec: int) -> String {
    if c is Section(let name, let configs) {
        let result = "[" + name.join(".") + "]\n";

        let i = 0;
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

impl fn (c: List<*Configuration>) get_section(name: List<String>) -> Option<Configuration> {
    let i = 0;
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

impl fn (c: List<*Configuration>) get_key_value(key: String) -> Option<Value> {
    let i = 0;
    while i < c.length {
        let config = *c[i];
        if config is KeyValue(let k, let v) && k == key {
            return Some(v);
        };
        i = i + 1;
    };

    return None;
}

impl fn 
    (c: List<*Configuration>)
    build_tree() -> List<*Configuration>
{
    if c.length == 0 {
        return [];
    };

    let first = c[0];
    let rest = c.slice(1, c.length);

    if (*first) is Section(let name, let _) {
        let sectionConfigs = &List.init<*Configuration>();
        let others = &List.init<*Configuration>();

        let i = 0;
        let is_correct = true;

        while i < rest.length {
            let config = rest[i];
            if (*config) is Section(let sectionName, let _) {
                if sectionName.join(".").starts_with(name.join(".")) && is_correct {
                    sectionConfigs.push(config);
                    is_correct = true;
                } else {
                    others.push(config);
                    is_correct = false;
                };
            } else if is_correct {
                sectionConfigs.push(config);
                is_correct = true;
            } else {
                others.push(config);
                is_correct = false;
            }
            i = i + 1;
        }
        let sectionConfigs_ = sectionConfigs->build_tree();

        let others_ = others->build_tree();

        let init_section = GC.allocate(Section(name, sectionConfigs_));

        return [init_section].extend(others_);
    }

    let rest_built = rest.build_tree();

    return [first].extend(rest_built);
}
