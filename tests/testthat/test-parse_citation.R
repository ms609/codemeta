test_that("We can parse_citation",{

  f <- test_path("test_examples", "RNeXML", "CITATION_")
  bib <- readCitationFile(f)
  schema <- lapply(bib, parse_citation)
  expect_type(schema, "list")
  expect_equal(schema[[1]][["@type"]], "ScholarlyArticle")

})

test_that("We can parse citations", {
  # prep
  examples_path <- test_path("test_examples")
  tmpdir <- tempdir()
  pkg_name <- "RNeXML"
  pkg_tmp_path <- file.path(tmpdir, pkg_name)
  on.exit(unlink(pkg_tmp_path, recursive = TRUE), add = TRUE)
  file.copy(file.path(examples_path, pkg_name), tmpdir, recursive = TRUE)
  file.rename(file.path(pkg_tmp_path, "CITATION_"), file.path(pkg_tmp_path, "CITATION"))

  ## CITATION in pkg root
  gc_root <- guess_citation(pkg_tmp_path)
  expect_type(gc_root, "list")
  expect_equal(gc_root[[1]][["@type"]], "ScholarlyArticle")

  ## CITATION in `inst`, i.e. in-dev pkg
  # prep
  dir.create(file.path(pkg_tmp_path, "inst"))
  file.rename(file.path(pkg_tmp_path, "CITATION"), file.path(pkg_tmp_path, "inst", "CITATION"))
  # ensure files are where we expect them to be
  expect_false(file.exists(file.path(pkg_tmp_path, "CITATION")))
  expect_true(file.exists(file.path(pkg_tmp_path, "inst", "CITATION")))
  # test
  gc_inst <- guess_citation(pkg_tmp_path)
  expect_type(gc_inst, "list")
  expect_equal(gc_inst[[1]][["@type"]], "ScholarlyArticle")

  ## not a package path returns null and doesn't error
  gc_null <- guess_citation(file.path(examples_path, "not-a-package"))
  expect_null(gc_null)
})

test_that("We can parse meta tags", {
  c <- test_path("test_examples", "rmarkdown", "CITATION_")
  # Need that file to completely parse all the tags
  d <- test_path("test_examples", "rmarkdown", "DESCRIPTION_codemeta_from_cran_")

  # Read DESCRIPTION to determine meta
  meta <- parse_package_meta(d)

  # Read and parse CITATION
  bib <- utils::readCitationFile(c, meta)
  expect_s3_class(bib, "citation")

  bibs <- lapply(bib, parse_citation)
  expect_type(bibs, "list")
  expect_snapshot_output({
    print("rmarkdown citation parsed with codemeta DESCRIPTION")
    print(bibs[1])
  })
})


test_that("We can parse citations with citation(auto = meta)", {
  c <- test_path("test_examples", "citation_auto", "CITATION_")

  # Need that file to completely parse all the tags
  d <- test_path("test_examples", "rmarkdown", "DESCRIPTION_codemeta_from_cran_")

  # Read DESCRIPTION to determine meta
  meta <- parse_package_meta(d)
  bib <- utils::readCitationFile(c, meta)
  expect_length(bib, 3)

  expect_s3_class(bib, "citation")

  bibs <- lapply(bib, parse_citation)
  expect_type(bibs, "list")
  expect_snapshot_output({
    print("citation with citation(auto = meta)")
    print(bibs)
  })
})

test_that("All BibTeX entry types are mapped", {
  # Capitalisation as normalised by utils::bibentry()
  bibtypes <- c("Article", "Book", "Booklet", "InBook", "InCollection",
                "InProceedings", "Manual", "MastersThesis", "Misc",
                "PhdThesis", "Proceedings", "TechReport", "Unpublished")
  for (bibtype in bibtypes) {
    expect_type(bibentry_to_schema_field(bibtype), "character")
  }
  expect_equal(bibentry_to_schema_field("incollection"),
               bibentry_to_schema_field("InCollection"))
})

test_that("InCollection is part of a Book", {
  bib <- bibentry(
    bibtype = "InCollection",
    booktitle = "Implementing Reproducible Computational Research",
    editor = "Victoria Stodden and Friedrich Leisch and Roger D. Peng",
    title = "knitr: A Comprehensive Tool for Reproducible Research in {R}",
    author = "Yihui Xie",
    publisher = "Chapman and Hall/CRC",
    year = "2014",
    isbn = "978-1466561595"
  )
  citation <- parse_citation(bib)
  expect_equal(citation[["@type"]], "CreativeWork")

  book <- citation$isPartOf
  expect_equal(book[["@type"]], "Book")
  expect_equal(book$name, "Implementing Reproducible Computational Research")
  expect_equal(vapply(book$editor, `[[`, "", "familyName"),
               c("Stodden", "Leisch", "Peng"))
  expect_equal(book$publisher$name, "Chapman and Hall/CRC")
  expect_equal(book$isbn, "978-1466561595")
})

test_that("InProceedings is part of a Book", {
  bib <- bibentry(
    bibtype = "InProceedings",
    booktitle = "Proceedings of the 2nd Workshop on Reproducibility",
    editor = "Ada Lovelace",
    title = "A paper",
    author = "Alan Turing",
    publisher = "ACM",
    year = "2020",
    pages = "1--10"
  )
  citation <- parse_citation(bib)
  expect_equal(citation[["@type"]], "ScholarlyArticle")
  expect_equal(citation$pagination, "1--10")

  book <- citation$isPartOf
  expect_equal(book[["@type"]], "Book")
  expect_equal(book$name, "Proceedings of the 2nd Workshop on Reproducibility")
  expect_equal(book$editor[[1]]$familyName, "Lovelace")
  expect_equal(book$publisher$name, "ACM")
})

test_that("InBook is part of a Book", {
  # BibTeX: title is the book's; chapter gives the chapter's number...
  bib <- bibentry(
    bibtype = "InBook",
    title = "The Book",
    chapter = "3",
    pages = "45--67",
    author = "Alan Turing",
    editor = "Ada Lovelace",
    publisher = "Chapman and Hall/CRC",
    year = "2000",
    isbn = "978-1466561595"
  )
  citation <- parse_citation(bib)
  expect_equal(citation[["@type"]], "Chapter")
  expect_null(citation$name)
  expect_identical(citation$position, 3L)
  expect_equal(citation$pagination, "45--67")
  book <- citation$isPartOf
  expect_equal(book[["@type"]], "Book")
  expect_equal(book$name, "The Book")
  expect_equal(book$editor[[1]]$familyName, "Lovelace")
  expect_equal(book$publisher$name, "Chapman and Hall/CRC")
  expect_equal(book$isbn, "978-1466561595")

  # ... or its name
  bib <- bibentry(
    bibtype = "InBook",
    title = "The Book",
    chapter = "Introduction",
    author = "Alan Turing",
    publisher = "Chapman and Hall/CRC",
    year = "2000"
  )
  citation <- parse_citation(bib)
  expect_equal(citation$name, "Introduction")
  expect_null(citation$position)
  expect_equal(citation$isPartOf$name, "The Book")

  # BibLaTeX: title is the chapter's; booktitle is the book's
  bib <- bibentry(
    bibtype = "InBook",
    title = "The Chapter",
    booktitle = "The Book",
    chapter = "3",
    author = "Alan Turing",
    publisher = "Chapman and Hall/CRC",
    year = "2000"
  )
  citation <- parse_citation(bib)
  expect_equal(citation$name, "The Chapter")
  expect_identical(citation$position, 3L)
  expect_equal(citation$isPartOf$name, "The Book")
})
