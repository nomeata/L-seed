<?php

	require_once('simpletest/unit_tester.php');
	require_once('simpletest/reporter.php');

	$test = &new GroupTest('All tests');
	$test->addTestFile('DatabaseTest.php');
	$test->addTestFile('BuisnessLogicTest.php');
	$test->run(new HtmlReporter());

?>
