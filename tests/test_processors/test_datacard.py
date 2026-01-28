"""Tests for the datacard processor."""

from pathlib import Path

from war_rig.processors.datacard import (
    UTILITY_PATTERNS,
    DatacardCatalog,
    DatacardInfo,
    DatacardProcessor,
    process_datacards,
)


class TestUtilityDetection:
    """Tests for utility type detection."""

    def test_detect_db2_unload(self) -> None:
        """Test detection of DB2 UNLOAD statements."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = """UNLOAD DIRECT NO FIXEDVARCHAR YES
SELECT TRK_NO, ACCT_ID FROM MUS.TE110_CTL_ACT_PYMT
WHERE LST_UPDT_TS < (CURRENT TIMESTAMP - 7 DAYS)"""

        utility_type, description = processor.detect_utility_type(content)
        assert utility_type == "DB2 UNLOAD"
        assert "unload" in description.lower()

    def test_detect_db2_unload_tablespace(self) -> None:
        """Test detection of UNLOAD TABLESPACE."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "UNLOAD TABLESPACE DBNAME.TSNAME"
        utility_type, _ = processor.detect_utility_type(content)
        assert utility_type == "DB2 UNLOAD"

    def test_detect_idcams_repro(self) -> None:
        """Test detection of IDCAMS REPRO statements."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = """REPRO -
  INFILE(SYSUT3) -
  OUTFILE(SYSUT4)"""

        utility_type, description = processor.detect_utility_type(content)
        assert utility_type == "IDCAMS REPRO"
        assert "REPRO" in description.upper() or "copy" in description.lower()

    def test_detect_repro_indataset(self) -> None:
        """Test detection of REPRO with INDATASET."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "REPRO INDATASET('MY.INPUT.FILE') OUTDATASET('MY.OUTPUT.FILE')"
        utility_type, _ = processor.detect_utility_type(content)
        assert utility_type == "IDCAMS REPRO"

    def test_detect_dfsort(self) -> None:
        """Test detection of DFSORT statements."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = """SORT FIELDS=(1,10,CH,A)
OUTREC FIELDS=(1,10,11X,11,20)"""

        utility_type, description = processor.detect_utility_type(content)
        assert utility_type == "DFSORT"
        assert "sort" in description.lower()

    def test_detect_icetool(self) -> None:
        """Test detection of ICETOOL statements."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = """ICETOOL
  SELECT FROM(INDD) TO(OUTDD) ON(1,10,CH) HIGHER"""

        utility_type, _ = processor.detect_utility_type(content)
        assert utility_type == "ICETOOL"

    def test_detect_idcams_define(self) -> None:
        """Test detection of IDCAMS DEFINE statements."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = """DEFINE CLUSTER(NAME(MY.VSAM.FILE) -
  VOLUMES(VOL001) -
  RECORDSIZE(80 80))"""

        utility_type, _ = processor.detect_utility_type(content)
        assert utility_type == "IDCAMS DEFINE"

    def test_detect_idcams_delete(self) -> None:
        """Test detection of IDCAMS DELETE statements."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "DELETE MY.OLD.FILE CLUSTER"
        utility_type, _ = processor.detect_utility_type(content)
        assert utility_type == "IDCAMS DELETE"

    def test_detect_idcams_listcat(self) -> None:
        """Test detection of IDCAMS LISTCAT statements."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "LISTCAT ENTRIES('MY.*.FILES') ALL"
        utility_type, _ = processor.detect_utility_type(content)
        assert utility_type == "IDCAMS LISTCAT"

    def test_detect_iebcopy(self) -> None:
        """Test detection of IEBCOPY statements."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "COPY OUTDD=OUTPUT,INDD=INPUT"
        utility_type, _ = processor.detect_utility_type(content)
        assert utility_type == "IEBCOPY"

    def test_detect_unknown(self) -> None:
        """Test that unknown content returns UNKNOWN type."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "SOME RANDOM CONTENT THAT MATCHES NO PATTERN"
        utility_type, description = processor.detect_utility_type(content)
        assert utility_type == "UNKNOWN"
        assert "unrecognized" in description.lower()

    def test_case_insensitive_detection(self) -> None:
        """Test that detection is case insensitive."""
        processor = DatacardProcessor(Path("."), Path("."))

        content_lower = "unload direct from table"
        content_upper = "UNLOAD DIRECT FROM TABLE"
        content_mixed = "Unload Direct From Table"

        type_lower, _ = processor.detect_utility_type(content_lower)
        type_upper, _ = processor.detect_utility_type(content_upper)
        type_mixed, _ = processor.detect_utility_type(content_mixed)

        assert type_lower == type_upper == type_mixed == "DB2 UNLOAD"


class TestReferenceExtraction:
    """Tests for dataset and table reference extraction."""

    def test_extract_dsn_references(self) -> None:
        """Test extraction of DSN= references."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "DSN=MY.INPUT.FILE DSN=MY.OUTPUT.FILE"
        datasets, tables = processor.extract_references(content)

        assert "MY.INPUT.FILE" in datasets
        assert "MY.OUTPUT.FILE" in datasets
        assert len(tables) == 0

    def test_extract_quoted_dsn(self) -> None:
        """Test extraction of quoted DSN references."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "DSN='MY.QUOTED.FILE'"
        datasets, _ = processor.extract_references(content)

        assert any("MY.QUOTED.FILE" in ds for ds in datasets)

    def test_extract_infile_outfile(self) -> None:
        """Test extraction of INFILE/OUTFILE DD references."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "INFILE(SYSUT1) OUTFILE(SYSUT2)"
        datasets, _ = processor.extract_references(content)

        assert "DD:SYSUT1" in datasets
        assert "DD:SYSUT2" in datasets

    def test_extract_table_references(self) -> None:
        """Test extraction of FROM table references."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "SELECT * FROM SCHEMA.TABLE_NAME WHERE X=1"
        _, tables = processor.extract_references(content)

        assert "SCHEMA.TABLE_NAME" in tables

    def test_extract_into_table_references(self) -> None:
        """Test extraction of INTO table references."""
        processor = DatacardProcessor(Path("."), Path("."))

        content = "LOAD INTO MYSCHEMA.TARGET_TABLE"
        _, tables = processor.extract_references(content)

        assert "MYSCHEMA.TARGET_TABLE" in tables


class TestDatacardCatalog:
    """Tests for the DatacardCatalog class."""

    def test_add_datacard(self) -> None:
        """Test adding a datacard to the catalog."""
        catalog = DatacardCatalog()
        info = DatacardInfo(
            file_path=Path("test.dc"),
            file_name="test.dc",
            utility_type="DB2 UNLOAD",
            description="Test unload",
            content_preview="UNLOAD DIRECT...",
            datasets=["MY.FILE"],
            tables=["SCHEMA.TABLE"],
        )

        catalog.add(info)

        assert catalog.total_count == 1
        assert catalog.unknown_count == 0
        assert "DB2 UNLOAD" in catalog.by_utility
        assert len(catalog.by_utility["DB2 UNLOAD"]) == 1

    def test_unknown_count(self) -> None:
        """Test that unknown datacards are counted correctly."""
        catalog = DatacardCatalog()
        info = DatacardInfo(
            file_path=Path("unknown.dc"),
            file_name="unknown.dc",
            utility_type="UNKNOWN",
            description="Unknown",
            content_preview="...",
        )

        catalog.add(info)

        assert catalog.total_count == 1
        assert catalog.unknown_count == 1

    def test_grouping_by_utility(self) -> None:
        """Test that datacards are grouped correctly by utility type."""
        catalog = DatacardCatalog()

        for i in range(3):
            catalog.add(DatacardInfo(
                file_path=Path(f"unload{i}.dc"),
                file_name=f"unload{i}.dc",
                utility_type="DB2 UNLOAD",
                description="Unload",
                content_preview="...",
            ))

        for i in range(2):
            catalog.add(DatacardInfo(
                file_path=Path(f"repro{i}.dc"),
                file_name=f"repro{i}.dc",
                utility_type="IDCAMS REPRO",
                description="Repro",
                content_preview="...",
            ))

        assert catalog.total_count == 5
        assert len(catalog.by_utility["DB2 UNLOAD"]) == 3
        assert len(catalog.by_utility["IDCAMS REPRO"]) == 2


class TestDatacardProcessor:
    """Tests for the DatacardProcessor class."""

    def test_process_file(self, tmp_path: Path) -> None:
        """Test processing a single datacard file."""
        dc_file = tmp_path / "test.dc"
        dc_file.write_text(
            "UNLOAD DIRECT NO SELECT COL1, COL2 FROM SCHEMA.MY_TABLE",
            encoding="utf-8",
        )

        processor = DatacardProcessor(tmp_path, tmp_path)
        info = processor.process_file(dc_file)

        assert info.file_name == "test.dc"
        assert info.utility_type == "DB2 UNLOAD"
        assert "SCHEMA.MY_TABLE" in info.tables
        assert len(info.content_preview) > 0

    def test_discover_datacards(self, tmp_path: Path) -> None:
        """Test discovery of datacard files."""
        # Create some .dc files
        (tmp_path / "file1.dc").write_text("UNLOAD DATA", encoding="utf-8")
        (tmp_path / "file2.dc").write_text("REPRO INFILE(X)", encoding="utf-8")
        (tmp_path / "subdir").mkdir()
        (tmp_path / "subdir" / "file3.dc").write_text("SORT FIELDS", encoding="utf-8")
        # Create non-datacard file
        (tmp_path / "other.txt").write_text("not a datacard", encoding="utf-8")

        processor = DatacardProcessor(tmp_path, tmp_path)
        datacards = processor.discover_datacards()

        assert len(datacards) == 3
        names = [dc.name for dc in datacards]
        assert "file1.dc" in names
        assert "file2.dc" in names
        assert "file3.dc" in names

    def test_process_all(self, tmp_path: Path) -> None:
        """Test processing all datacards."""
        (tmp_path / "unload1.dc").write_text(
            "UNLOAD DIRECT FROM SCHEMA.TABLE1",
            encoding="utf-8",
        )
        (tmp_path / "unload2.dc").write_text(
            "UNLOAD TABLE FROM SCHEMA.TABLE2",
            encoding="utf-8",
        )
        (tmp_path / "repro1.dc").write_text(
            "REPRO INFILE(IN) OUTFILE(OUT)",
            encoding="utf-8",
        )

        processor = DatacardProcessor(tmp_path, tmp_path)
        catalog = processor.process_all()

        assert catalog.total_count == 3
        assert "DB2 UNLOAD" in catalog.by_utility
        assert len(catalog.by_utility["DB2 UNLOAD"]) == 2
        assert "IDCAMS REPRO" in catalog.by_utility
        assert len(catalog.by_utility["IDCAMS REPRO"]) == 1

    def test_generate_catalog_markdown(self, tmp_path: Path) -> None:
        """Test generation of markdown catalog."""
        (tmp_path / "test.dc").write_text(
            "UNLOAD DIRECT SELECT * FROM SCHEMA.TABLE",
            encoding="utf-8",
        )

        processor = DatacardProcessor(tmp_path, tmp_path)
        processor.process_all()
        markdown = processor.generate_catalog_markdown()

        assert "# Datacard Catalog" in markdown
        assert "## Summary" in markdown
        assert "## DB2 UNLOAD" in markdown
        assert "test.dc" in markdown
        assert "| Utility | Count |" in markdown

    def test_write_catalog(self, tmp_path: Path) -> None:
        """Test writing the catalog file."""
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        output_dir.mkdir()

        (input_dir / "test.dc").write_text("SORT FIELDS=(1,10,CH,A)", encoding="utf-8")

        processor = DatacardProcessor(input_dir, output_dir)
        output_path = processor.write_catalog()

        assert output_path == output_dir / "DATACARDS.md"
        assert output_path.exists()

        content = output_path.read_text(encoding="utf-8")
        assert "DFSORT" in content

    def test_empty_directory(self, tmp_path: Path) -> None:
        """Test processing an empty directory."""
        processor = DatacardProcessor(tmp_path, tmp_path)
        catalog = processor.process_all()

        assert catalog.total_count == 0
        assert len(catalog.by_utility) == 0


class TestProcessDatacardsFunction:
    """Tests for the convenience function."""

    def test_process_datacards_with_files(self, tmp_path: Path) -> None:
        """Test the convenience function with files present."""
        (tmp_path / "test.dc").write_text("DEFINE CLUSTER(NAME(X))", encoding="utf-8")

        output_path = process_datacards(tmp_path, tmp_path)

        assert output_path is not None
        assert output_path.exists()
        assert output_path.name == "DATACARDS.md"

    def test_process_datacards_empty(self, tmp_path: Path) -> None:
        """Test the convenience function with no files."""
        output_path = process_datacards(tmp_path, tmp_path)

        assert output_path is None


class TestUtilityPatterns:
    """Tests for utility pattern coverage."""

    def test_all_patterns_have_required_fields(self) -> None:
        """Test that all patterns have name, pattern, and description."""
        for pattern in UTILITY_PATTERNS:
            assert pattern.name, "Pattern missing name"
            assert pattern.pattern, "Pattern missing regex"
            assert pattern.description, "Pattern missing description"

    def test_pattern_uniqueness(self) -> None:
        """Test that all pattern names are unique."""
        names = [p.name for p in UTILITY_PATTERNS]
        assert len(names) == len(set(names)), "Duplicate pattern names found"
