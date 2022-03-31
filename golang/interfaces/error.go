package interfaces

// The error built-in interface type is the conventional interface for
// representing an error condition, with the nil value representing no error.
type error interface {
	Error() string
}

// Implementation of error as defined in the error package in the Go Standard Library.
// Each call to New returns a distinct value even if the texts are identical.
func New(text string) error {
	return &errorString{text}
}

type errorString struct {
	s string
}

// Fulfills the Error interface.
func (e *errorString) Error() string {
	return e.s
}
