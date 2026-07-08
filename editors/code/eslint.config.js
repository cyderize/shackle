const tsParser = require("@typescript-eslint/parser")
const tsPlugin = require("@typescript-eslint/eslint-plugin")

module.exports = [
	{
		ignores: ["node_modules/**", "out/**"],
	},
	{
		files: ["src/**/*.ts", "src/**/*.tsx"],
		languageOptions: {
			ecmaVersion: "latest",
			sourceType: "module",
			parser: tsParser,
		},
		plugins: {
			"@typescript-eslint": tsPlugin,
		},
		rules: {
			...tsPlugin.configs.recommended.rules,
			semi: [2, "never"],
			"@typescript-eslint/no-unused-vars": 0,
			"@typescript-eslint/no-explicit-any": 0,
			"@typescript-eslint/explicit-module-boundary-types": 0,
			"@typescript-eslint/no-non-null-assertion": 0,
		},
	},
]
