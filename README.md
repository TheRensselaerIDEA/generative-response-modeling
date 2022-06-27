# generative-response-modeling
Code and data for [our paper](https://arxiv.org/abs/2204.04353): Should we tweet this? Generative response modeling for predicting reception of public health messaging on Twitter

![Experiment Setup](experiment_setup.png)

# Getting Started
Follow this guide to:
- Interact with our trained response generation models
- Import our COVID-19 and Vaccines public health tweet datasets and run the model evaluation from the paper

Our models are also available on the HuggingFace model hub:
- COVID-19: [TheRensselaerIDEA/gpt2-large-covid-tweet-response](https://huggingface.co/TheRensselaerIDEA/gpt2-large-covid-tweet-response)
- Vaccines: [TheRensselaerIDEA/gpt2-large-vaccine-tweet-response](https://huggingface.co/TheRensselaerIDEA/gpt2-large-vaccine-tweet-response)

## 1. Setup
### 1.1 Clone or download this repository & install Python dependencies
We recommend creating an environment with Python 3.7 or greater. 

If PyTorch is not already installed in your environment, please install the appropriate configuration of PyTorch for you environment (OS, CUDA version) before proceeding - see [https://pytorch.org/get-started/locally/](https://pytorch.org/get-started/locally/).

To install python dependencies run:
```shell
pip install -r requirements.txt
```

### 1.2 Start the response distribution prediction Flask server
Depending on the model you wish to use (COVID-19 or Vaccines), specify the appropriate config:
```shell
# COVID-19
python response_prediction/predict_distributions.py -c covid19_config.json
# ...OR...
# Vaccines
python response_prediction/predict_distributions.py -c vaccines_config.json
```

## 2. Interact with trained models
Open [analysis/predict_responses.Rmd](analysis/predict_responses.Rmd) in a [knitr](https://yihui.org/knitr/) enabled R environment. We recommend using [RStudio](https://rstudio.com/).

Knit or run the notebook to generate responses. Specifically, the notebook includes list parameters `prompt_authors`, `prompt_messages`, and `response_sample_size`. It generates a sample of N=`response_sample_size` responses for each message in `prompt_messages` for each author account in `prompt_authors`. Generated responses are assigned sentiment scores, and each sample is output with response text and sentiment statistics.

## 3. Import the COVID-19 and Vaccines datasets
### 3.1 Install Elasticsearch
Elasticsearch [version 7.x](https://www.elastic.co/downloads/past-releases/elasticsearch-7-17-4) is required to import the datasets and run the evaluation. Elasticsearch v8.x may also work but we have not tested with it.

### 3.2 Get Twitter developer account (skip if you already have one)
Importing the datasets requires downloading tweets by ID from the Twitter API. A Twitter developer account is required for this.
If you don't have one already, you can apply at [https://developer.twitter.com/en/apply-for-access](https://developer.twitter.com/en/apply-for-access).

### 3.3 Import and pre-process the tweets
We re-use the tweet collection pipeline from our [previous paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8378598/). The code required to import the datasets has been included here, and the [import instructions](https://github.com/TheRensselaerIDEA/COVID-masks-nlp/blob/master/README.md#2-setup) from the accompanying repository are compatible. Specifically, make sure to follow the following sections when importing each dataset:
- [Section 2.2.1](https://github.com/TheRensselaerIDEA/COVID-masks-nlp/blob/master/README.md#221-configure-your-twitter-credentials-and-elasticsearch-settings) and [Section 2.2.2](https://github.com/TheRensselaerIDEA/COVID-masks-nlp/blob/master/README.md#222-import-tweets-from-a-list-of-tweet-ids) to import the tweets.
- [Section 2.3](https://github.com/TheRensselaerIDEA/COVID-masks-nlp/blob/master/README.md#23-compute-embeddings-and-sentiment-scores-for-the-imported-tweets) to pre-process the tweets (compute embeddings and sentiment scores).

Importantly, use the following modifications to the original instructions:
  - In sections 2.2.1 and 2.3 use:
    - `"elasticsearch_index_name": "covid19-pubhealth-responses"` for the COVID-19 dataset
    - `"elasticsearch_index_name": "vaccine-pubhealth-responses"` for the Vaccines dataset
  - In section 2.2.2 use:
    - `--datasetglob=./../tweet_ids/covid19/*/*.txt` for the COVID-19 dataset
    - `--datasetglob=./../tweet_ids/vaccine/*/*.txt` for the Vaccines dataset

## 4. Run the evaluation notebook
Open [analysis/model_eval.Rmd](analysis/model_eval.Rmd) in a [knitr](https://yihui.org/knitr/) enabled R environment. We recommend using [RStudio](https://rstudio.com/).

Knit or run the notebook to sample responses and evaluate the baselines \& model as described in the paper.
Specifically:
- For the COVID-19 dataset, set:
  - `elasticsearch_index <- "covid19-pubhealth-responses"`
  - `rangestart <- "2020-03-01 00:00:00"`
  - `rangeend <- "2020-10-01 00:00:00"`
- For the Vaccines dataset, set:
  - `elasticsearch_index <- "vaccine-pubhealth-responses"`
  - `rangestart <- "2021-10-01 00:00:00"`
  - `rangeend <- "2022-02-01 00:00:00"`

# Reference
If you use our data, models, or code in your work, please cite:
```
@article{sanders2022should,
  title={Should we tweet this? Generative response modeling for predicting reception of public health messaging on Twitter},
  author={Sanders, Abraham and Ray-Majumder, Debjani and Erickson, John S and Bennett, Kristin P},
  journal={arXiv preprint arXiv:2204.04353},
  year={2022}
}
```