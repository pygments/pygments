int id() {
    id:
    int i = true ? 1 : 2;
    default_value:

    switch (2) {
        case Qfalse:
            break;
        case Qnil: case foo: case std::bar():
            return Qnil;
        default:
            return 0;
    }
}
