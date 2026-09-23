# codemeta (development version)

* CITATION entry types are matched case-insensitively, so `InBook`,
  `InCollection`, `InProceedings`, `MastersThesis`, `PhdThesis` and
  `TechReport` entries now get a schema.org `@type`; `InBook`, `InCollection`
  and `InProceedings` entries record their book or proceedings as `isPartOf`,
  and a BibTeX-style `InBook` entry, whose `title` is that of the book, is
  identified by its `chapter` field (#8).

# codemeta 0.1.1

* Fix handling of multiple URLs [#1]
* Added a `NEWS.md` file to track changes to the package.
