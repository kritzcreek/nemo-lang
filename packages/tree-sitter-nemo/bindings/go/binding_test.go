package tree_sitter_nemo_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-nemo"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_nemo.Language())
	if language == nil {
		t.Errorf("Error loading Nemo grammar")
	}
}
