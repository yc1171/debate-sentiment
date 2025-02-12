<h1 align="center"> Measuring Emotion in Parliamentary Debates </h1>
<h3 align="center"> A Replication Study with Automated Textual Analysis(1973- 1977) </h3>  


<!-- INTRODUCTIONS -->
<h2 id="overview">Overview</h2>

<p>
This repository contains our replication of <a href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0168843">
<em>Measuring Emotion in Parliamentary Debates with Automated Textual Analysis</em></a> by Rheault et al. (2016). 
The original study applies natural language processing techniques to analyze emotional polarity in British parliamentary debates over a century. 
The study constructs a domain-specific polarity lexicon and investigates how sentiment trends align with political and economic events.
</p>

<ul>
  <li><strong>Original Paper:</strong> <a href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0168843">PLoS ONE (2016)</a></li>
  <li><strong>Dataset:</strong> <a href="https://github.com/lrheault/emotion">Available on GitHub</a></li>
</ul>

<br>



<!-- MOTIVATION -->
<h2 id="motivation-for-replication">Motivation for Replication</h2>

<p>
We selected this paper for replication because:
</p>

<ul>
  <li>It demonstrates the use of <strong>domain-specific sentiment lexicons</strong>, a critical technique in text analysis.</li>
  <li>It explores <strong>temporal sentiment trends</strong>, allowing comparisons with real-world political and economic events.</li>
  <li>It provides a <strong>structured approach to sentiment aggregation</strong>, useful for applications in text representation and computational social science.</li>
</ul>

<p>
Our replication exercise aims to verify the original findings and explore potential variations using <strong>updated methodologies and datasets</strong>.
</p>


<br>


<!-- DATASET -->
<h2 id="dataset">Dataset</h2>

<p>
The original study by Rheault et al. (2016) analyzed debates from the <a href="http://data.politicalmashup.nl/parldumps/uk/">British House of Commons’ Hansard</a> spanning from <strong>1909 to 2013</strong>. 
The dataset consists of digitized parliamentary records, covering speeches, oral questions, and answers and keeps updates until now. 
To capture sentiment, the authors constructed a domain-specific polarity lexicon using word embeddings (GloVe) trained on this corpus.
</p>

<h3>Our Approach</h3>
<ul>
  <li><strong>Time Frame:</strong> While the original study spans over a century with more than 5 million records, we focus on debates from <strong>1973–1977</strong> to analyze sentiment during a specific historical period. The paper highlights the major recession of 1973–1975 as a notable period of negativity in parliamentary speeches.
It links this period to labor conflicts, economic downturns, and inflation spikes.</li>
  <li><strong>Lexicon Adaptation:</strong> We reconstruct and evaluate the domain-specific polarity lexicon using alternative word embedding techniques.</li>
  <li><strong>Sentiment Aggregation:</strong> We explore different sentiment scoring and normalization techniques to compare with the original results.</li>
  <li><strong>Alternative Data Sources:</strong> We examine potential extensions, including external economic and political indicators, to further validate sentiment trends.</li>
</ul>


<br>


<!-- PREREQUISITES -->
<h2 id="prerequisites"> Prerequisites</h2>

<!--This project is written in Python programming language. <br>-->
The following open source packages are used in this project:
* tidyverse
* tidyr
* lubridate
* scales
* stringr
* tidytext
* ggplot2
* gridExtra
* quenteda
* quanteda.sentiment
