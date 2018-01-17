library(ape)

context("correct step counting")
test_that("right counting", {
    ## Tree
    tree <- read.tree(text = "((((((1,2),3),4),5),6),(7,(8,(9,(10,(11,12))))));")
    characters <- c("23--1??--032", # 0
                    "1---1111---1", # 1
                    "1100----1100", # 2
                    "11-------100", # 3
                    "----1111---1", # 4
                    "01----010101", # 5
                    "01---1010101", # 6
                    "1??--??--100", # 7
                    "21--3??--032", # 8
                    "11--1??--111", # 9
                    "11--1000001-", # 10
                    "01------0101", # 11
                    "110--?---100", # 12
                    "11--1??--111", # 13
                    "210--100--21", # 14
                    "????----1???", # 15
                    "23--1----032", # 16
                    "1----1----1-", # 17
                    "-1-1-1--1-1-", # 18
                    "23--1??--032", # 19
                    "--------0101", # 20
                    "10101-----01", # 21
                    "011--?--0011", # 22
                    "110--??--100", # 23
                    "11--1000001-", # 24
                    "21--1----012", # 25
                    "11----111111", # 26
                    "10101-----01", # 27
                    "210210------", # 28
                    "----1111----", # 29
                    "230--??1--32", # 30
                    "023--??1--32", # 31
                    "023-???1--32", # 32
                    "23--1?1--023", # 33
                    "----1010----", # 34
                    "------11---1", # 35
                    "10----11---1", # 36
                    "320--??3--21") # 37
    ## Results
    expected_results <- c(5, 2, 3, 2, 1, 5, 5, 2, 5, 2, 2, 4, 3, 2, 5, 0, 5, 2, 4, 5, 2, 4, 3, 3, 2, 5, 1, 4, 4, 0, 5, 5, 4, 5, 2, 1, 3, 5)

    ## Run the tests
    for(test in 1:length(characters)) {
        # print(test)
        suppressWarnings(output <- apply.reconstruction(tree, characters[test], passes = 4, method = "NA", inapplicable = NULL))

        tree_score <- score.from(output$regions) + score.from(output$changes)

        # if(tree_score != expected_results[test]) {
        #     print(paste("Failed", test))
        # }

        expect_equal(tree_score, expected_results[test])
    }

    ## Run the bigger tree tests
    tree <- read.tree(text = "((1,2),((3,(4,5)),(6,(7,(8,(9,(10,((11,(12,(13,(14,15)))),(16,(17,(18,(19,20))))))))))));")
    characters <- c("11111---111---11---1" # 1
                    )
    ## Results
    expected_results <- c(3)

    ## Run the tests
    for(test in 1:length(characters)) { 
        suppressWarnings(output <- apply.reconstruction(tree, characters[test], passes = 4, method = "NA", inapplicable = NULL))
        # if(matrix$score != expected_results[test]) {
        #     print(paste("test", test, "failed"))
        #     print(paste("Is", matrix$score, "instead of", expected_results[test]))
        # }
        tree_score <- score.from(output$regions) + score.from(output$changes)
        expect_equal(tree_score, expected_results[test])
    }




    # ## Run one bigger ambiguous tree
    tree <- read.tree(text = "(a,(((b,(c,((((d,(e,(f,g))),(h,i)),(j,k)),(((l,m),(n,(o,p))),((q,r),s))))),(t,u)),(v,(w,x))));")
    characters <- c("33?-?-3-???????-3???--33")

    ## Results
    expected_results <- c(1)

    ## Run the test
    for(test in 1:length(characters)) { 
        suppressWarnings(output <- apply.reconstruction(tree, characters[test], passes = 4, method = "NA", inapplicable = NULL))
        # if(matrix$score != expected_results[test]) {
        #     print(paste("test", test, "failed"))
        #     print(paste("Is", matrix$score, "instead of", expected_results[test]))
        # }
        tree_score <- score.from(output$regions) + score.from(output$changes)
        expect_equal(tree_score, expected_results[test])
    }

})
