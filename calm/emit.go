// calm/emit.go
package calm

import "io"

// Emit writes the CALM document for a source model. The node set is exactly the
// resource graph's logical resources (one node per infra.Resource), so the emitted
// document is provider-independent, the same equivalence witness infra gates on.
func Emit(m Model, w io.Writer) error {
	data, err := Marshal(m.ToDoc())
	if err != nil {
		return err
	}
	_, err = w.Write(data)
	return err
}
