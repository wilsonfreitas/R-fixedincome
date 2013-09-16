library(RUnit)

suite <- defineTestSuite("fixedincome",
	dirs =  '.',
	testFileRegexp = "^test-.+\\.R",
	testFuncRegexp = "^test.+")

result <- runTestSuite(suite)
printTextProtocol(result)


