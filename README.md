# LDA spell-sound typology

Data and scripts for analysis used for Finding structure in Spelling and Pronunciation using Latent Dirichlet Allocation presented in NLP30/2024

## Data
Text files

### Spell
1. [English spells (.csv)](data-words/base-spell-English-r6e-original.csv)
2. [French spells (.csv)](data-words/base-spell-French-r0-1k-mc.csv)
3. [German spells (.csv)](data-words/base-spell-German-r1a-original.csv)
4. [Russian spells (.csv)](data-words/base-spell-Russian-r0-1k-mc.csv)
5. [Swahili spells (.csv)](data-words/base-spell-Swahili-r0-1k-mc.csv)

### Sound
6. [English sounds (.csv)](data-words/base-sound-English-r6e-original.csv)
7. [French sounds (.csv)](data-words/base-sound-French-r0-opendic-s900.csv)
8. [German sounds (.csv)](data-words/base-sound-German-r1a-originals.csv)


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

