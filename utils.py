
'''
Last revision: 09/09/2025
Author: Guillermo Martinez 

'''

from pathlib import Path

from pdf2image import convert_from_path
from PIL import Image
import torch
import tabula
import pandas as pd



def report_tables_to_png(report: str, first_page: int = 71, last_page: int = 75):
    ''' 
    Convert the tables in the reports to images.

    Parameters
    ----------
    report : str
        Filename of the PDF report located in ./data/.
    first_page : int, optional
        First page number to extract (default: 71).
    last_page : int, optional
        Last page number to extract (default: 75).
    
    '''
    print("Converting the tables to images...")

    pdf_path =  report
    output_path = Path("data/pngs")
    output_path.mkdir(parents=True, exist_ok=True)

    images = convert_from_path(pdf_path, first_page=first_page, last_page=last_page)

    for i, page_num in enumerate(range(first_page, last_page + 1)):
            page_image = images[i]
            page_image.save(output_path / f"{page_num}.png", "PNG")

    print("Image conversion complete.")



def obtain_tensors(image_processor, model, input_dir: str = "data/pngs", output_dir: str = "data/tensors", threshold: float = 0.5) -> None:
    ''' 
    Detect table boundaries in report page images using a trained model.

    Parameters
    ----------
    image_processor : object
        Preprocessing utility (e.g., from HuggingFace).
    model : object
        Trained model used for object detection.
    input_dir : str, optional
        Path to folder with PNG images (default: ./data/pngs).
    output_dir : str, optional
        Path to save tensor results (default: ./data/tensors).
    '''
    print("Detecting table boundaries...")

    folder_path = Path(input_dir)
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    for file_name in sorted(folder_path.glob("*.png")):
        image = Image.open(file_name).convert("RGB")
        inputs = image_processor(images=image, return_tensors="pt")

        # Model forward pass
        outputs = model(**inputs)

        # Extract bounding boxes
        target_sizes = torch.tensor([image.size[::-1]])
        results = image_processor.post_process_object_detection(
            outputs, threshold=threshold, target_sizes=target_sizes
        )[0]

        torch.save(results["boxes"], output_path / f"{file_name.stem}.pt")

    print("Tensor extraction complete.")



def report_tables_to_csv(res_cons: float, pdf_file: str, input_dir: str = "data/tensors", output_dir: str = "data/csvs") -> None:
    ''' 
    Extract tables from a PDF using bounding boxes and save them as CSV files.

    Parameters
    ----------
    res_cons : float
        Scaling factor to adjust bounding box coordinates.
    pdf_file : str
        Filename of the PDF report located in ./data/.
    input_dir : str, optional
        Directory with tensor files (default: ./data/tensors).
    output_dir : str, optional
        Directory to save CSV files (default: ./data/csv).
    '''
    print("Extracting tables and saving them as .csv files...")

    pdf_path = pdf_file
    folder_path = Path(input_dir)
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    for file_name in sorted(folder_path.glob("*.pt")):
            table_tensor = torch.load(file_name, weights_only=True)

            for idx, box in enumerate(table_tensor):
                xmin, ymin, xmax, ymax = [coord * res_cons for coord in box.tolist()]
                page = int(file_name.stem)

                tables = tabula.read_pdf(
                    str(pdf_path),
                    pages=page,
                    stream=True,
                    area=[ymin, xmin - 5, ymax, xmax + 5],
                )

                if tables:
                    df = pd.concat(tables, ignore_index=True)
                    df.to_csv(output_path / f"{page}_{idx}.csv", index=False)

    print("CSV extraction complete.")



def concatenate_csvs(input_dir: str = "data/csvs", output_filename: str = "coffeeshops.csv") -> None:
    ''' 
    Concatenate all CSV files into a single dataset.

    Parameters
    ----------
    input_dir : str, optional
        Directory containing CSV files (default: ./data/csv).
    output_filename : str, optional
        Filename of the concatenated output CSV (default: coffeeshops.csv).
    '''

    print("Concatenating all the .csv files into a single one...")

    folder_path = Path(input_dir)
    output_path = Path("data") / output_filename

    csv_files = [f for f in folder_path.glob("*.csv") if f.name != output_filename]
    df_list = [pd.read_csv(f) for f in csv_files]

    final_df = pd.concat(df_list, ignore_index=True)

    # Adjust column names (hardcoded)
    final_df.columns = [
        "Gemeente", "1999", "2000", "2001", "2002", "2003", "2004",
        "2005", "2006", "2007", "2009", "2011", "2012", "2013", "2014",
        "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"
    ]

    final_df.to_csv(output_path, index=False)
    print(f"Concatenation complete: {output_path}")

