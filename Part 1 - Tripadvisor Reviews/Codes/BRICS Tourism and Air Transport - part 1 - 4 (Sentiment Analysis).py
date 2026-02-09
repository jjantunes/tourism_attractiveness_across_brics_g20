###############################################################################
## Libraries
###############################################################################

## Libraries
import numpy  as np
import pandas as pd

## Functions
from transformers import pipeline, AutoTokenizer
from os           import chdir
from time         import time

###############################################################################
## Paths, seeds and options
###############################################################################

## Paths
path_root    = r"C:/Users/Jorge/OneDrive/Pos Graduacao/Mestrado/PEP/BRICS Tourism and Air Transport/Part 1 - Tripadvisor Reviews/"
path_data    = f"{path_root}Data/"           
path_results = f"{path_root}Results/"

###############################################################################
## Load dataset
###############################################################################

## Change directory
chdir(path_results)

## Read dataset
df_reviews = pd.read_parquet("Database - Reviews Tripadvisor.parquet")

## Titles and Texts
indexes = df_reviews["Index"].tolist()
titles  = df_reviews["Title"].tolist()
texts   = df_reviews["Text" ].tolist()

###############################################################################
## Texts manipulation
###############################################################################

## Model name
model_name = "cardiffnlp/twitter-roberta-base-sentiment-latest"

## Tokenizer from model
tokenizer = AutoTokenizer.from_pretrained(model_name)

## Initital time
time_ini = time()

## Encode texts into tokens
encoded = tokenizer(
    text                      = texts,
    max_length                = 512  ,
    truncation                = True ,
    padding                   = True ,
    stride                    = 100  ,
    return_overflowing_tokens = True ,
    return_tensors            = "pt" 
)

## Chunks and titles
chunks_index  = [indexes[i] for i in encoded["overflow_to_sample_mapping"]]
chunks_title  = [titles [i] for i in encoded["overflow_to_sample_mapping"]]
chunks_text   = [tokenizer.decode(input_ids, skip_special_tokens = True) for i, input_ids in enumerate(encoded["input_ids"])]
chunks_tokens = encoded["attention_mask"].sum(dim=1).tolist()

## List of dits to coerce as data frame
list_chunks = [
    {"Index": index ,"Title": title, "Chunk": chunk, "Tokens_Chunk": token} 
    for index, title, chunk, token in zip(chunks_index, chunks_title, chunks_text, chunks_tokens)
]

## Coerce to data frame
df_chunks = pd.DataFrame(list_chunks)

## Add total tokens per title
df_chunks["Tokens_Total"] = df_chunks.groupby("Index")["Tokens_Chunk"].transform("sum")

## Add weights
df_chunks["Tokens_Weights"] = df_chunks["Tokens_Chunk"] / df_chunks["Tokens_Total"]

## Evaluation time
eval_time_text_manipulation = time() - time_ini

## Print evaluation time
print(f"Evaluation time (Text manipulation): {eval_time_text_manipulation:.2f} seconds")

###############################################################################
## Sentiment analysis with roBERTa
###############################################################################

## Sentiment analysis pipeline
sentiment_pipeline = pipeline(task = "sentiment-analysis", model = model_name, device = 0)

## Initial time
time_ini = time()

## Prediction
res = sentiment_pipeline(df_chunks["Chunk"].tolist(), top_k = None)

## Evaluation time
eval_time_sentiment = time() - time_ini

## Print evaluation time
print(f"Evaluation time (Sentiment analysis): {eval_time_sentiment:.2f} seconds")

###############################################################################
## Result data frame
###############################################################################

## Coerce to data frame
df_res = pd.DataFrame([
    {"Index"         : df_chunks["Index"         ][i]                     ,
     "Title"         : df_chunks["Title"         ][i]                     ,
     "Chunk"         : df_chunks["Chunk"         ][i]                     ,
     "Tokens_Chunks" : df_chunks["Tokens_Chunk"  ][i]                     ,
     "Tokens_Total"  : df_chunks["Tokens_Total"  ][i]                     ,
     "Tokens_Weights": df_chunks["Tokens_Weights"][i]                     ,
     "Negative"      : {d['label']: d['score'] for d in x}.get('negative'),
     "Neutral"       : {d['label']: d['score'] for d in x}.get('neutral' ),
     "Positive"      : {d['label']: d['score'] for d in x}.get('positive')}
     for i, x in enumerate(res)                                            
])

## Weighted mean sentiment
df_negative = df_res.groupby('Index').apply(lambda x: np.average(x['Negative'], weights = x['Tokens_Weights']), include_groups = False)
df_neutral  = df_res.groupby('Index').apply(lambda x: np.average(x['Neutral' ], weights = x['Tokens_Weights']), include_groups = False)
df_positive = df_res.groupby('Index').apply(lambda x: np.average(x['Positive'], weights = x['Tokens_Weights']), include_groups = False)

## Reset index
df_negative = df_negative.reset_index() 
df_neutral  = df_neutral .reset_index() 
df_positive = df_positive.reset_index() 

## Rename columns
df_negative.columns = ['Index', 'Negative']
df_neutral .columns = ['Index', 'Neutral' ]
df_positive.columns = ['Index', 'Positive']

## Sentiment data frame
df_sentiment = df_negative.merge(df_neutral, on = 'Index').merge(df_positive, on = 'Index')

## Add sentiment label
df_sentiment['Sentiment_Label'] = df_sentiment[['Negative', 'Neutral', 'Positive']].idxmax(axis = 1)

## Add sentiment score (between 0 and 1)
df_sentiment['Sentiment_Index'] = df_sentiment['Positive'] - df_sentiment['Negative']

## Merge data with reviews
df_reviews = df_reviews.merge(df_sentiment, on = 'Index', how = 'left')

###############################################################################
## Data frame with Tourism Attraction Index
###############################################################################

## Quantity of reviews per country
df_countries_rev = (
    df_reviews.groupby(["Country", "Attraction"])
    .apply(lambda x: np.max(x["Qnt_rev"]), include_groups = False)
    .reset_index()
    .groupby("Country")
    .apply(lambda x: np.sum(x[0]), include_groups = False)
    .reset_index()
)

## Change names
df_countries_rev.columns = ["Country", "Total_Qnt_Rev"]

## Merge data
df_reviews = df_reviews.merge(df_countries_rev, on = "Country", how = "left")

## Add Attraction Weight
df_reviews["Attraction_Weight"] = df_reviews["Qnt_rev"] / df_reviews["Total_Qnt_Rev"]

## Create Tourism Attraction Index per country
df_tourism_attraction_index = (
    df_reviews
    .groupby("Country")
    .apply(lambda x: np.average(x["Sentiment_Index"], weights = x["Attraction_Weight"]), include_groups = False)
    .reset_index()
)

## Change names
df_tourism_attraction_index.columns = ["Country", "Tourism_Attraction_Index"]

###############################################################################
## Save data
###############################################################################

## Change directory
chdir(path_results)

## Save data
df_res                     .to_excel("Database - Reviews Chunks Sentiment.xlsx"     , index = False)
df_reviews                 .to_excel("Database - Reviews Tripadvisor Sentiment.xlsx", index = False)
df_tourism_attraction_index.to_excel("Database - Tourism Attraction Index.xlsx"     , index = False)
