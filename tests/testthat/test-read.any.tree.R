context("read.any.tree")

## Test
test_that("read.any.tree works", {

    ## Testing trees
    newick <- "(((Strix_aluco:4.2,Asio_otus:4.2):3.1,Athene_noctua:7.3):6.3,Tyto_alba:13.5);"
    cat(newick, file = "example_newick.tre", sep = "\n")
    multi_newick <- paste(rep(newick, 3), sep = "\n")
    cat(multi_newick, file = "example_multi_newick.tre", sep = "\n")
    ape::write.nexus(ape::rtree(3), file = "example_nexus.nex")
    ape::write.nexus(ape::rmtree(2,3), file = "example_multi_nexus.nex")

    ## Right output
    expect_is(
        read.any.tree("example_newick.tre")
        , "phylo")
    expect_is(
        read.any.tree("example_nexus.nex")
        , "phylo")
    expect_is(
        read.any.tree("example_multi_newick.tre")
        , "multiPhylo")
    expect_is(
        read.any.tree("example_multi_nexus.nex")
        , "multiPhylo")    

    unlink("example_newick.tre")
    unlink("example_multi_newick.tre")
    unlink("example_nexus.nex")
    unlink("example_multi_nexus.nex")
})
