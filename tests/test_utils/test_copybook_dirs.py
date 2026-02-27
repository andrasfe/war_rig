"""Tests for war_rig.utils.copybook_dirs."""

from pathlib import Path

import pytest

from war_rig.utils.copybook_dirs import clear_cache, derive_copybook_dirs


@pytest.fixture(autouse=True)
def _clear_cache():
    """Ensure the module cache is empty for each test."""
    clear_cache()
    yield
    clear_cache()


def test_derive_from_flat_dir(tmp_path: Path) -> None:
    """Files in a flat directory produce one entry."""
    (tmp_path / "PROG.cbl").write_text("hello")
    (tmp_path / "COPY.cpy").write_text("world")

    result = derive_copybook_dirs(tmp_path)
    assert result == [tmp_path]


def test_derive_from_nested_dirs(tmp_path: Path) -> None:
    """Files in subdirectories produce entries for each parent."""
    sub1 = tmp_path / "a" / "b"
    sub1.mkdir(parents=True)
    (sub1 / "X.cpy").write_text("x")

    sub2 = tmp_path / "c"
    sub2.mkdir()
    (sub2 / "Y.cbl").write_text("y")

    result = derive_copybook_dirs(tmp_path)
    result_strs = {str(d) for d in result}
    assert str(sub1) in result_strs
    assert str(sub2) in result_strs


def test_empty_dir_returns_empty(tmp_path: Path) -> None:
    result = derive_copybook_dirs(tmp_path)
    assert result == []


def test_extra_dirs_appended(tmp_path: Path) -> None:
    (tmp_path / "PROG.cbl").write_text("x")
    extra = tmp_path / "extra"
    extra.mkdir()

    result = derive_copybook_dirs(tmp_path, extra_dirs=[extra])
    result_strs = {str(d) for d in result}
    assert str(extra) in result_strs
    assert str(tmp_path) in result_strs


def test_extra_dirs_deduplication(tmp_path: Path) -> None:
    (tmp_path / "PROG.cbl").write_text("x")

    result = derive_copybook_dirs(tmp_path, extra_dirs=[tmp_path])
    # tmp_path should appear only once
    assert len([d for d in result if str(d) == str(tmp_path)]) == 1


def test_cache_reused(tmp_path: Path) -> None:
    (tmp_path / "A.cbl").write_text("a")
    r1 = derive_copybook_dirs(tmp_path)
    # Add a new file — cached result won't include it
    (tmp_path / "B.cbl").write_text("b")
    r2 = derive_copybook_dirs(tmp_path)
    assert r1 == r2


def test_clear_cache_resets(tmp_path: Path) -> None:
    (tmp_path / "A.cbl").write_text("a")
    r1 = derive_copybook_dirs(tmp_path)
    clear_cache()
    (tmp_path / "subdir").mkdir()
    (tmp_path / "subdir" / "B.cbl").write_text("b")
    r2 = derive_copybook_dirs(tmp_path)
    assert len(r2) > len(r1)
