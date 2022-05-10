type Value = f32;

pub struct ValueSet {
    values: Vec<Value>,
}

impl ValueSet {
    pub fn new() -> ValueSet {
        ValueSet {
            values: Vec::new(),
        }
    }

    pub fn write(&mut self, value: Value) {
        self.values.push(value);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.values.iter()
    }

    pub fn get(&self, index: usize) -> &Value {
        &self.values[index]
    }
}
