package generics

type Int interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64
}

func Abs[T Int](v T) T {
	if v < 0 {
		return -v
	} else {
		return v
	}
}

func Equal[T comparable](a, b T) bool {
	return a == b
}

func PtrOf[T any](v T) *T {
	return &v
}
