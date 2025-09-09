
import argparse
from transformers import AutoImageProcessor, TableTransformerForObjectDetection
from utils import (
    report_tables_to_png,
    obtain_tensors,
    report_tables_to_csv,
    concatenate_csvs,
)

def main(pdf_file: str, first_page: int, last_page: int, dpi: int = 200, threshold: float = 0.5) -> None:
    '''
    Run the complete pipeline: PDF → PNG → Tensors → CSV → Aggregated CSV

    Parameters
    ----------
    pdf_file : str
        Path to the input PDF file
    first_page : int
        First page to process
    last_page : int
        Last page to process
    dpi : int, optional
        Resolution used for scaling bounding boxes (default: 200) 
    '''

    # Step 1: Convert report tables to PNG
    report_tables_to_png(pdf_file, first_page, last_page)

    # Step 2: Initialize processor and model
    image_processor = AutoImageProcessor.from_pretrained(
        "microsoft/table-transformer-detection"
    )
    model = TableTransformerForObjectDetection.from_pretrained(
        "microsoft/table-transformer-detection"
    )

    # Step 3: Detect tables (tensors)
    obtain_tensors(image_processor, model, threshold=threshold)

    # Step 4: Extract tables to CSV
    res_cons = 72 / dpi
    report_tables_to_csv(res_cons, pdf_file)

    # Step 5: Concatenate CSVs
    concatenate_csvs()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Extract and process tables from the Dutch coffeeshops report."
    )
    parser.add_argument(
        "pdf_file",
        type=str,
        help="Path to the input PDF file (inside ./data/).",
    )
    parser.add_argument(
        "--first_page",
        type=int,
        default=71,
        help="First page number to extract (default: 71).",
    )
    parser.add_argument(
        "--last_page",
        type=int,
        default=75,
        help="Last page number to extract (default: 75).",
    )
    parser.add_argument(
        "--dpi",
        type=int,
        default=200,
        help="DPI resolution for bounding box scaling (default: 200).",
    )
    parser.add_argument(
        "--threshold",
        type=float,
        default=0.5,
        help="Model threshold for table detection (default: 0.5).",
    )

    args = parser.parse_args()
    main(args.pdf_file, args.first_page, args.last_page, args.dpi)