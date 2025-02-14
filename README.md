<h1 align="center"> Measuring Emotion in Parliamentary Debates </h1>
<h3 align="center"> A Replication Study with Automated Textual Analysis(1973-1977) </h3>  


<!-- INTRODUCTIONS -->
<h2 id="overview">Overview</h2>

<p>
This repository contains our partial replication of ["Measuring Emotion in Parliamentary Debates with Automated Textual Analysis"](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0168843) by Rheault et al. (2016). The original study examined the relationship between economic conditions and sentiments in parliamentary discourse by constructing a domain-specific lexicon and analyzing temporal patterns in political discourse. Our replication focuses on validating their sentiment measurement approach using multiple general-purpose lexicons, with a specific focus on the 1973-1977 period.

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
  <li>It demonstrates the case where use of a<strong>domain-specific sentiment lexicon</strong> would be particularly necessary when the content of interest has certain inherent characteristics not captured by a general-purpose lexicon, and preserving those features would be relevant for the study.</li>
  <li>Its provides a structured approach to aggregate <strong>temporal sentiment trends</strong> with comparisons of real-world economic events, which can be adapted in a wide range of quesries in computational social science. </li>
</ul>

We focus on replicating only a specific component of the original study because the original paper employs complex time series econometrics that require extensive statistical expertise. The 1973-1977 period also offers quite a clear test case due to significant economic events that allows us to compare the performance of multiple lexicons.

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

<br>

<!-- PROJECT FILES -->
<h2 id="project-files">Project Files</h2>

<p>
This repository contains datasets, scripts, and output files for our replication study. 
Our replication includes modifications and additional analysis, detailed below.
</p>

---

<h3> Data</h3>

<ul>
  <li><code>emotion-final-q.csv</code> - Final dataset (quarterly, normalized variables).</li>
  <li><code>emotion-final-y.csv</code> - Final dataset (yearly, normalized variables).</li>
  <li><code>lexicon-polarity.csv</code> - Domain-specific polarity lexicon (4200 words).</li>
</ul>

---

<h3> Scripts</h3>

<ul>
  <li><code>replication-script.r</code> - R script for replicating the sentiment analysis and generating outputs.</li>
</ul>

---

<h3> Output</h3>

<ul>
  <li><code>AFINN_senti_comp.png</code> - Sentiment comparison using the AFINN lexicon.</li>
  <li><code>BING_senti_comp.png</code> - Sentiment comparison using the BING lexicon.</li>
  <li><code>LSD_senti_comp.png</code> - Sentiment comparison using the LSD lexicon.</li>
  <li><code>comp_with_author.png</code> - Comparison of our sentiment analysis results with the original authors' findings.</li>
  <li><code>senti_misery.png</code> - Sentiment trends compared with the misery index.</li>
  <li><code>senti_unemp.png</code> - Sentiment trends compared with unemployment rates.</li>
</ul>

---

<h3> Modifications and Extensions </h3>

<ul>
  <li>Reproduced the original sentiment analysis using updated scripts.</li>
  <li>Generated sentiment comparison plots with multiple lexicons (AFINN, BING, LSD).</li>
  <li>Performed sentiment correlation analysis with economic indicators (misery index, unemployment).</li>
  <li>Validated results by comparing with the original author's dataset.</li>
</ul>

<br>

<!-- CONTRIBUTORS -->
<h2 id="contributors">Contributors</h2>

<p>
This replication study was conducted as part of the replication exercise for 
PPOL 6801 - Text as Data (Spring 2025) at 
<a href="https://mccourt.georgetown.edu/">Georgetown University, McCourt School of Public Policy</a>.
</p>

<ul>
  <li><strong>Irene Chen</strong> - <a href="mailto:yc1171@georgetown.edu">yc1171@georgetown.edu</a></li>
  <li><strong>Tian Tong</strong> - <a href="mailto:yt583@georgetown.edu">yt583@georgetown.edu</a></li>
</ul>

