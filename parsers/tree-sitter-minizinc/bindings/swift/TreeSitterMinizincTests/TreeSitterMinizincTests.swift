import XCTest
import SwiftTreeSitter
import TreeSitterMinizinc

final class TreeSitterMinizincTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_minizinc())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Minizinc grammar")
    }
}
