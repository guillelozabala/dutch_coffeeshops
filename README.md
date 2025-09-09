#  Coffeeshops in the Netherlands

This repository **extracts Table 1 from Breuer & Intraval’s 2022 coffeeshops report** and converts it into a CSV file. If you only need the data, the resulting file is located at `data/coffeeshops.csv`, so you don’t need to run the code.

The table reports the number of coffeeshops in each Dutch municipality from 1999 to 2022, annually.


---

##  Source

The original report, *Coffeeshops in Nederland 2022: Aantallen coffeeshops en gemeentelijk beleid 1999–2022*, published by Breuer & Intraval, is available from the author’s website:

> [Coffeeshops in Nederland 2022](https://breuerintraval.nl/publicatie/coffeeshops-in-nederland-2022/).


##  Workflow

The workflow automates the following steps:

1. Convert selected PDF pages into PNG images
2. Detect table boundaries using HuggingFace’s Table Transformer for object detection 
3. Extract tables from the PDF using bounding boxes and save each as a CSV file 
4. Concatenate individual CSVs into one final, aggregated dataset


##  Requirements

- **Python 3.9+**
- **Java** (required by tabula-py)
- Install the required packages:

    ```
    pip install -r requirements.txt
<!-- - -->

## Usage 

From the repository root, run:

    python main.py data/BI-Coffeeshops-in-Nederland-2022-juni-2023-webversie.pdf --first_page 71 --last_page 75 --dpi 200 --threshold 0.5

You can override default arguments when applying the code to other reports or other page ranges.

* `pdf_file` (positional, str): Path to the input PDF

* `--first_page`(int, default: 71): First page in the PDF to process (inclusive).

* `--last_page` (int, default: 75): Last page in the PDF to process (inclusive).

* `--dpi` (int, default: 200): Resolution used to compute the scaling constant for Tabula.
The code uses `res_cons = 72 / dpi` to map detector coordinates to PDF points.
<!-- Keep this consistent with how the page images were generated. -->

* `--threshold` (float, default: 0.5): Confidence threshold for table detection.
The lower it is the more boxes you detect, but get more exposed to false positives.

## License

**MIT License**. See `LICENSE` file for details.