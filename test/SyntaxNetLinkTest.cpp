#include <gtest/gtest.h>
#include "../src/ParseTree.h"
#include "../src/SyntaxNetLink.h"

TEST(ParserLinkTest, simpleTest1) {

    SyntaxNetLink linky;

    ParseTree parsed = linky.parse("Let there be 15 cubes.");

    // Check whether the root word was parsed correctly
    ASSERT_EQ(parsed.getRootWord().getRootWord(), "Let");

    ASSERT_EQ(parsed.getRootWord().getRole(), "ROOT");

    ASSERT_EQ(parsed.getRootWord().getNature(), "VB");

}

TEST(ParserLinkTest, simpleTest2) {

    SyntaxNetLink linky;

    ParseTree parsed = linky.parse("The quick brown fox jumps over the lazy dog.");

    // Check whether the root word was parsed correctly
    ASSERT_EQ(parsed.getRootWord().getRootWord(), "jumps");

    ASSERT_EQ(parsed.getRootWord().getRole(), "ROOT");

    ASSERT_EQ(parsed.getRootWord().getNature(), "VB");

}

TEST(ParserLinkTest, simpleTestGardenPath) {

    SyntaxNetLink linky;

    ParseTree parsed = linky.parse("The old man the boat");

    // Check whether the root word was parsed correctly
    ASSERT_EQ(parsed.getRootWord().getRootWord(), "man");

    ASSERT_EQ(parsed.getRootWord().getRole(), "ROOT");

    ASSERT_EQ(parsed.getRootWord().getNature(), "VB");

}