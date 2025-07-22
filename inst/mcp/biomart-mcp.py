# biomart-mcp.py
# Source: https://github.com/jzinno/biomart-mcp
# Copyright (c) 2024 Joseph Zinno
# Licensed under the MIT License (see inst/mcp/biomart-mcp.LICENSE)
#
# This file is included in the wizrd package under the terms of the MIT license.
#
# /// script
# dependencies = [
#   "mcp[cli]",
#   "pybiomart",
# ]
# ///

"""
A MCP server to interface with Biomart, providing tools to query biological data.

This server implements the Model Context Protocol (MCP) to access the Ensembl Biomart
database, allowing querying of genomic annotations, gene identifiers, and other
biological data through a standardized interface.
"""

import sys
import time

import pybiomart

from functools import lru_cache
from mcp.server.fastmcp import FastMCP

DEFAULT_HOST = "http://www.ensembl.org"
MAX_RETRIES = 3
RETRY_DELAY = 2  # seconds

COMMON_ATTRIBUTES = [
    "ensembl_gene_id",
    "external_gene_name",
    "hgnc_symbol",
    "hgnc_id",
    "gene_biotype",
    "ensembl_transcript_id",
    "ensembl_peptide_id",
    "ensembl_exon_id",
    "description",
    "chromosome_name",
    "start_position",
    "end_position",
    "strand",
    "band",
    "transcript_start",
    "transcript_end",
    "transcription_start_site",
    "transcript_length",
]

mcp = FastMCP("Biomart")


@lru_cache()
def get_server():
    """Create and cache a server connection with error handling"""
    try:
        return pybiomart.Server(host=DEFAULT_HOST)
    except Exception as e:
        print(f"Error connecting to Biomart server: {str(e)}", file=sys.stderr)
        raise


@mcp.tool()
def list_marts():
    """
    Lists all available Biomart marts (databases) from Ensembl.

    Biomart organizes biological data in a hierarchy: MART -> DATASET -> ATTRIBUTES/FILTERS.
    This function returns all available marts as a CSV string.

    Returns:
        str: CSV-formatted table of all marts with their display names and descriptions.

    Example:
        list_marts()
        >>> "name,display_name,description
             ENSEMBL_MART_ENSEMBL,Ensembl Genes,Gene annotation from Ensembl
             ENSEMBL_MART_MOUSE,Mouse strains,Strain-specific data for mouse
             ..."
    """
    try:
        server = get_server()
        return server.list_marts().to_csv(index=False).replace("\r", "")
    except Exception as e:
        print(f"Error listing marts: {str(e)}", file=sys.stderr)
        return f"Error: {str(e)}"


@mcp.tool()
def list_datasets(mart: str):
    """
    Lists all available biomart datasets for a given mart.

    Each mart contains multiple datasets. This function returns all datasets
    available in the specified mart as a CSV string.

    Args:
        mart (str): The mart identifier to list datasets from.
            Valid values include: ENSEMBL_MART_ENSEMBL, ENSEMBL_MART_MOUSE,
            ENSEMBL_MART_ONTOLOGY, ENSEMBL_MART_GENOMIC, ENSEMBL_MART_SNP,
            ENSEMBL_MART_FUNCGEN

    Returns:
        str: CSV-formatted table of all datasets with their display names and descriptions.

    Example:
        list_datasets("ENSEMBL_MART_ENSEMBL")
        >>> "name,display_name,description
             hsapiens_gene_ensembl,Human genes,Human genes (GRCh38.p13)
             mmusculus_gene_ensembl,Mouse genes,Mouse genes (GRCm39)
             ..."
    """
    try:
        server = get_server()
        return server[mart].list_datasets().to_csv(index=False).replace("\r", "")
    except Exception as e:
        print(f"Error listing datasets for mart {mart}: {str(e)}", file=sys.stderr)
        return f"Error: {str(e)}"


@mcp.tool()
def list_common_attributes(mart: str, dataset: str):
    """
    Lists commonly used attributes available for a given dataset.

    This function returns only the most frequently used attributes (defined in COMMON_ATTRIBUTES)
    to avoid overwhelming the model with too many options. For a complete list,
    use list_all_attributes.

    Args:
        mart (str): The mart identifier (e.g., "ENSEMBL_MART_ENSEMBL")
        dataset (str): The dataset identifier (e.g., "hsapiens_gene_ensembl")

    Returns:
        str: CSV-formatted table of common attributes with their display names and descriptions.

    Example:
        list_common_attributes("ENSEMBL_MART_ENSEMBL", "hsapiens_gene_ensembl")
        >>> "name,display_name,description
             ensembl_gene_id,Gene stable ID,Ensembl stable ID for the gene
             external_gene_name,Gene name,The gene name
             ..."
    """
    server = pybiomart.Server(host=DEFAULT_HOST)
    df = server[mart][dataset].list_attributes()
    df = df[df["name"].isin(COMMON_ATTRIBUTES)]
    return df.to_csv(index=False).replace("\r", "")


@mcp.tool()
def list_all_attributes(mart: str, dataset: str):
    """
    Lists all available attributes for a given dataset with some filtering.

    This function returns a filtered list of all attributes available for the specified
    dataset. Some less commonly used attributes (homologs, microarray probes) are
    filtered out to reduce the response size.

    CAUTION: This function can return a large number of attributes and may be unstable
    for certain datasets. Consider using list_common_attributes first.

    Args:
        mart (str): The mart identifier (e.g., "ENSEMBL_MART_ENSEMBL")
        dataset (str): The dataset identifier (e.g., "hsapiens_gene_ensembl")

    Returns:
        str: CSV-formatted table of all filtered attributes.

    Example:
        list_all_attributes("ENSEMBL_MART_ENSEMBL", "hsapiens_gene_ensembl")
    """
    server = pybiomart.Server(host=DEFAULT_HOST)
    df = server[mart][dataset].list_attributes()
    df = df[~df["name"].str.contains("_homolog_", na=False)]
    df = df[~df["name"].str.contains("dbass", na=False)]
    df = df[~df["name"].str.contains("affy_", na=False)]
    df = df[~df["name"].str.contains("agilent_", na=False)]
    return df.to_csv(index=False).replace("\r", "")


@mcp.tool()
def list_filters(mart: str, dataset: str):
    """
    Lists all available filters for a given dataset.

    Filters are used to narrow down the results of a Biomart query.
    This function returns all filters that can be applied to the specified dataset.

    Args:
        mart (str): The mart identifier (e.g., "ENSEMBL_MART_ENSEMBL")
        dataset (str): The dataset identifier (e.g., "hsapiens_gene_ensembl")

    Returns:
        str: CSV-formatted table of all filters with their display names and descriptions.

    Example:
        list_filters("ENSEMBL_MART_ENSEMBL", "hsapiens_gene_ensembl")
        >>> "name,description
             chromosome_name,Chromosome/scaffold name
             start,Gene start (bp)
             end,Gene end (bp)
             ..."
    """
    server = pybiomart.Server(host=DEFAULT_HOST)
    return server[mart][dataset].list_filters().to_csv(index=False).replace("\r", "")


@mcp.tool()
def get_data(mart: str, dataset: str, attributes: list[str], filters: dict[str, str]):
    """
    Queries Biomart for data using specified attributes and filters.

    This function performs the main data retrieval from Biomart, allowing you to
    query biological data by specifying which attributes to return and which filters
    to apply. Includes automatic retry logic for resilience.

    Args:
        mart (str): The mart identifier (e.g., "ENSEMBL_MART_ENSEMBL")
        dataset (str): The dataset identifier (e.g., "hsapiens_gene_ensembl")
        attributes (list[str]): List of attributes to retrieve (e.g., ["ensembl_gene_id", "external_gene_name"])
        filters (dict[str, str]): Dictionary of filters to apply (e.g., {"chromosome_name": "1"})

    Returns:
        str: CSV-formatted results of the query.

    Example:
        get_data(
            "ENSEMBL_MART_ENSEMBL",
            "hsapiens_gene_ensembl",
            ["ensembl_gene_id", "external_gene_name", "chromosome_name"],
            {"chromosome_name": "X", "biotype": "protein_coding"}
        )
        >>> "ensembl_gene_id,external_gene_name,chromosome_name
             ENSG00000000003,TSPAN6,X
             ENSG00000000005,TNMD,X
             ..."
    """
    for attempt in range(MAX_RETRIES):
        try:
            server = get_server()
            return (
                server[mart][dataset]
                .query(attributes=attributes, filters=filters)
                .to_csv(index=False)
                .replace("\r", "")
            )
        except Exception as e:
            print(
                f"Error getting data (attempt {attempt+1}/{MAX_RETRIES}): {str(e)}",
                file=sys.stderr,
            )
            if attempt < MAX_RETRIES - 1:
                print(f"Retrying in {RETRY_DELAY} seconds...", file=sys.stderr)
                time.sleep(RETRY_DELAY)
            else:
                return f"Error: {str(e)}"


@lru_cache()
def _get_translation_dict(mart: str, dataset: str, from_attr: str, to_attr: str):
    """
    Helper function to get and cache a translation dictionary.
    """
    try:
        server = get_server()
        dataset_obj = server[mart][dataset]
        df = dataset_obj.query(attributes=[from_attr, to_attr])
        return dict(zip(df.iloc[:, 0], df.iloc[:, 1]))
    except Exception as e:
        print(f"Error getting translation dictionary: {str(e)}", file=sys.stderr)
        return {}


@mcp.tool()
def get_translation(mart: str, dataset: str, from_attr: str, to_attr: str, target: str):
    """
    Translates a single identifier from one attribute type to another.

    This function allows conversion between different identifier types, such as
    converting a gene symbol to an Ensembl ID. Results are cached to improve performance.

    Args:
        mart (str): The mart identifier (e.g., "ENSEMBL_MART_ENSEMBL")
        dataset (str): The dataset identifier (e.g., "hsapiens_gene_ensembl")
        from_attr (str): The source attribute name (e.g., "hgnc_symbol")
        to_attr (str): The target attribute name (e.g., "ensembl_gene_id")
        target (str): The identifier value to translate (e.g., "TP53")

    Returns:
        str: The translated identifier, or an error message if not found.

    Example:
        get_translation("ENSEMBL_MART_ENSEMBL", "hsapiens_gene_ensembl", "hgnc_symbol", "ensembl_gene_id", "TP53")
        >>> "ENSG00000141510"
    """
    try:
        result_dict = _get_translation_dict(mart, dataset, from_attr, to_attr)
        if target not in result_dict:
            print(f"Target '{target}' not found in translation", file=sys.stderr)
            return f"Error: Target '{target}' not found"
        return result_dict[target]
    except Exception as e:
        print(f"Error in translation: {str(e)}", file=sys.stderr)
        return f"Error: {str(e)}"


@mcp.tool()
def batch_translate(mart: str, dataset: str, from_attr: str, to_attr: str, targets: list[str]):
    """
    Translates multiple identifiers in a single batch operation.

    This function is more efficient than multiple calls to get_translation when
    you need to translate many identifiers at once.

    Args:
        mart (str): The mart identifier (e.g., "ENSEMBL_MART_ENSEMBL")
        dataset (str): The dataset identifier (e.g., "hsapiens_gene_ensembl")
        from_attr (str): The source attribute name (e.g., "hgnc_symbol")
        to_attr (str): The target attribute name (e.g., "ensembl_gene_id")
        targets (list[str]): List of identifier values to translate (e.g., ["TP53", "BRCA1", "BRCA2"])

    Returns:
        dict: A dictionary containing:
            - translations: Dictionary mapping input IDs to translated IDs
            - not_found: List of IDs that could not be translated
            - found_count: Number of successfully translated IDs
            - not_found_count: Number of IDs that could not be translated

    Example:
        batch_translate("ENSEMBL_MART_ENSEMBL", "hsapiens_gene_ensembl", "hgnc_symbol", "ensembl_gene_id", ["TP53", "BRCA1", "BRCA2"])
        >>> {"translations": {"TP53": "ENSG00000141510", "BRCA1": "ENSG00000012048"}, "not_found": ["BRCA2"], "found_count": 2, "not_found_count": 1}
    """
    # Use the cached helper function to get the translation dictionary
    result_dict = _get_translation_dict(mart, dataset, from_attr, to_attr)

    translations = {}
    not_found = []

    for target in targets:
        if target in result_dict:
            translations[target] = result_dict[target]
        else:
            not_found.append(target)

    if not_found:
        print(
            f"The following targets were not found: {', '.join(not_found)}",
            file=sys.stderr,
        )

    return {
        "translations": translations,
        "not_found": not_found,
        "found_count": len(translations),
        "not_found_count": len(not_found),
    }


if __name__ == "__main__":
    mcp.run()
