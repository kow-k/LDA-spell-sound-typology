# LDA spell-sound typology

Data and scripts for analysis used for Finding structure in Spelling and Pronunciation using Latent Dirichlet Allocation presented in NLP30/2024

## Data
Text files

1. [English spells (.csv)](data-words/base-spell-English-r6e-originals.csv)
2. [French spells (.csv)](data-words/base-spell-French-r0-1k-mc.csv)
3. [German spells (.csv)](data-words/base-spell-German-r1a-originals.csv)
4. [Russian spells (.csv)](data-words/base-spell-Russian-r0-1k-mc.csv)
5. [Swahili spells (.csv)](data-words/base-spell-Swahili-r0-1k-mc.csv)

## Scripts for data analysis
Scripts for analysis (Jupyter notebooks)

1. [LDA spell/sound clusterer (Jupyter notebook)](LDA-spell-sound.ipynb)

## Prerequisites
Needed Python packages

1. pyLDAvis [recommended to install first of all]
2. WordCloud
3. plotly
4. adjustText

## Results
Results in .html

1. [results for LDA clustering of mixed spells (#topics: 5)](results/spell-ntop5)
2. [results for LDA clustering of mixed spells (#topics: 15)](results/spell-ntop15)
3. [results for LDA clustering of mixed sounds (#topics: 5)](results/sound-ntop5)
4. [results for LDA clustering of mixed sounds (#topics: 15)](results/sound-ntop15)

