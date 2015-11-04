default:
	java -jar tajs-all.jar example.js -collect-variable-info

flow:
	java -jar tajs-all.jar example.js -flowgraph

richards:
	java -jar tajs-all.jar richards.js -collect-variable-info > variables.richards.out
