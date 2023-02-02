build:
	elm-codegen run --flags-from="public/translations.en.json"
	elm make src/Main.elm --output=public/main.bundle.js --debug

test:
	elm-review
	elm-test