#include <gtest/gtest.h>
#include "../src/ParseTree.h"

TEST(ParseTreeTest,parseTest) {

    // Parse a tree
    ParseTree tree(
            R"(Let VB ROOT
 +-- be VB ccomp
     +-- there EX expl
     +-- cubes NNS nsubj
         +-- 15 CD num)");

    // Check whether the root word was parsed correctly
    ASSERT_EQ(tree.getRootWord().getRootWord(), "Let");

    ASSERT_EQ(tree.getRootWord().getRole(), "ROOT");

    ASSERT_EQ(tree.getRootWord().getNature(), "VB");

    // Randomly check tree structure
    ASSERT_EQ(tree.getRootWord().children.size(), 1);

    ASSERT_EQ(tree.getRootWord().children[0].children.size(), 2);

    // Check correct parsing of one of the children
    ASSERT_EQ(tree.getRootWord().children[0].children[1].getRootWord(), "cubes");

    ASSERT_EQ(tree.getRootWord().children[0].children[1].getRole(), "nsubj");

    ASSERT_EQ(tree.getRootWord().children[0].children[1].getNature(), "NNS");


}

TEST(ParseTreeTest,parseTestOnlyRoot) {

    ParseTree tree(
            R"(Let VB ROOT)");

    ASSERT_EQ(tree.getRootWord().getRootWord(), "Let");

    ASSERT_EQ(tree.getRootWord().getRole(), "ROOT");

    ASSERT_EQ(tree.getRootWord().getNature(), "VB");

    ASSERT_EQ(tree.getRootWord().children.size(), 0);

}