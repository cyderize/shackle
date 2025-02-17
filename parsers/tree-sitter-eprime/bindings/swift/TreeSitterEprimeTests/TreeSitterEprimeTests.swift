import XCTest
import SwiftTreeSitter
import TreeSitterEprime

final class TreeSitterEprimeTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_eprime())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Eprime grammar")
    }
}
