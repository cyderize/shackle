import XCTest
import SwiftTreeSitter
import TreeSitterDatazinc

final class TreeSitterDatazincTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_datazinc())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Datazinc grammar")
    }
}
