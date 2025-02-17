package tree_sitter_eprime_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_eprime "git+github.com/shackle-rs/shackle.git/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_eprime.Language())
	if language == nil {
		t.Errorf("Error loading Eprime grammar")
	}
}
