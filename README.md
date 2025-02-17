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
  <li>The almost perfect </strong>counter-cyclical pattern</strong> between sentiment and economic indicators raises interesting questions about sentiment measurement in political text </li>
  <li>It provides an opportunity to </strong>examine trade-offs</strong> between domain-specific and general-purpose sentiment lexicons </li>
  <li>The 1973-1977 period offers a clear test case with significant economic events </li>
</ul>

We focus on replicating only a specific component of the original study because the original paper employs complex time series econometrics that require extensive statistical expertise. The 1973-1977 period presents a robust test case, marked by significant economic events that enable a comparative analysis of lexicon performance.

<p>
Our replication exercise aims to verify the original findings and explore potential variations using <strong>updated methodologies and datasets</strong>.
</p>


<br>


<!-- DATASET -->
<h2 id="dataset">Dataset</h2>

<p>
The original study by Rheault et al. (2016) analyzed debates from the <a href="http://data.politicalmashup.nl/parldumps/uk/">British House of Commons’ Hansard</a> spanning from <strong>1909 to 2013</strong>. 
The dataset consists of digitized parliamentary records, including speeches, oral questions, and answers. The dataset has been continuously updated.
To capture sentiment, the authors constructed a domain-specific polarity lexicon using word embeddings (GloVe) trained on this corpus.
</p>

<h3>Our Approach</h3>
<ul>
  <li><strong>Time Frame:</strong> While the original study spans over a century with more than 5 million records, we focus on debates from <strong>1973–1977</strong> to analyze sentiment during a specific historical period. The paper highlights the major recession of 1973–1975 as a notable period of negativity in parliamentary speeches. It links this period to labor conflicts, economic downturns, and inflation spikes.</li>
  <li><strong>Lexicon Comparison:</strong> We compare four different lexicons:</li>
  <li>- Authors' custom parliamentary lexicon</li>
  <li>- AFINN (-5 to +5 integer scores)</li>
  <li>- Bing (binary positive/negative)</li>
  <li>- LSD2015 (designed for political text)</li>
  <li><strong>Methodology:</strong> We follow the authors' minimal preprocessing approach while exploring how different lexicons handle parliamentary language.</li>
</ul>


<br>


<!-- PREREQUISITES -->
<h2 id="prerequisites"> Prerequisites</h2>

<!--This project is written in R programming language. <br>-->
The following R packages are required:
* tidyverse
* tidyr
* lubridate
* scales
* stringr
* tidytext
* ggplot2
* gridExtra
* quanteda
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
  <li><code>emotion-final-q.csv</code> - Quarterly aggregated data for polarity score and economic indicators.</li>
  <li><code>emotion-final-y.csv</code> - Yearly aggregated data for polarity score and economic indicators.</li>
  <li><code>lexicon-polarity.csv</code> - Authors' domain-specific lexicon (4200 words).</li>
  <li><code>uk_hansard_1973_1977.csv</code> - Raw debate text for replication (subset on year 1973-1977).</li>
</ul>

---

<h3> Scripts</h3>

<ul>
  <li><code>replication-script.r</code> - Main analysis script </li>
  <li><code>exploration.Rmd</code> -  Initial data exploration </li>
  <li><code>lexicon_comparison.Rmd</code> - Detailed lexicon analysis </li>
</ul>

---

<h3> Outputs</h3>

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
  <li>Validated results by comparing with the original author's dataset.</li>
  <li>Generated sentiment comparison plots with multiple lexicons (AFINN, BING, LSD).</li>
  <li>Examined word-level scoring differences across lexicons.</li>
</ul>

<br>

<!-- CONTRIBUTORS -->
<h2 id="contributors">Contributors</h2>

<p>
This replication study was conducted as part of the replication exercise for 
PPOL 6801 - Text as Data (Spring 2025) at 
<a href="https://mccourt.georgetown.edu/">Georgetown University, McCourt School of Public Policy</a>.
</p>

We thank the original authors for making their data and code publicly available, and Professor Nejla Asimovic for guidance on this replication exercise.

<ul>
  <li><strong>Irene Chen</strong> - <a href="mailto:yc1171@georgetown.edu">yc1171@georgetown.edu</a></li>
  <li><strong>Tian Tong</strong> - <a href="mailto:yt583@georgetown.edu">yt583@georgetown.edu</a></li>
</ul>

