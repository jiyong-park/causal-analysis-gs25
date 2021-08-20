# [인과추론 연습문제] GS25 를 둘러싼 남혐 논란과 매출 변화에 대한 인과적 분석

<인과추론 서머세션>에서 강의 자료로 준비한 사례로서, 최근에 언론과 온라인 커뮤니티에서 회자되었던 GS25 를 둘러싼 남혐 논란과 2021년 2분기 GS25 의 매출 감소 현상에 대해서 분석함으로써 인과적 사고와 데이터 분석의 중요성에 대해 조명해보고자 합니다. 가급적 해당 기업에 관한 시사/경영 이슈에 대한 가치 판단은 모두 배제하고 데이터 분석 결과만 논의할 예정이며, 분석에 활용된 데이터와 R 코드는 모두 공유되어 있으니 자유롭게 테스트 해보시길 바랍니다.
#### 본문을 읽기 전에 아래 질문에 대해서 먼저 생각해 보시길 추천 드립니다.
1. GS25 를 둘러싼 이슈는 인과추론 문제인가?
2. 인과적인 효과를 어떻게 정의하고 측정할 수 있을까?
3. GS25 와 CU 를 비교하는 건 합당할까? 만약 아니라면, 대안은 무엇인가?
4. 분석 결과를 어떻게 신뢰할 수 있을까?<br/><br/>


## 1.	GS25 를 둘러싼 이슈는 인과추론 문제인가?
-	원인과 결과를 분석하는 인과추론 문제는 데이터의 특성이나 데이터 분석 방법론에 의해 결정되지 않습니다. 예를 들어, 대표적인 통계모형 중 하나인 회귀분석(regression)은 인과추론에도 활용할 수 있고 동시에 예측 모델로 활용할 수도 있습니다.
-	인과추론 문제는 데이터 분석의 목적에 따라 정의될 수 있을 것입니다. 원인에 대한 개입과 조정을 통해 결과를 개선하는 것이 목적이라면 인과추론(causal inference) 문제라고 볼 수 있고, (목표치에 가까운) 최선의 결과를 도출하는 것이 목적이라면 예측(prediction) 문제로 정의할 수 있습니다. 더 나아가, 결과를 극대화하기 위해서 한정된 자원(예산) 하에서 원인에 대한 개입 전략이나 예측값을 최적화하는 의사결정은 처방(prescription) 문제로 정의할 수 있습니다.
-	2021년 2분기의 GS25 실적에 많은 사람들이 관심을 갖졌던 이유는 무엇일까요? 물론 사람들에 따라 다르겠지만, 이를 적극적으로 비판하거나 옹호하는 사람들은 “온라인 상에서 논란이 되었던 남혐 논란과 불매 운동”에 대한 인과적 효과를 이해함으로써 그러한 움직임의 확대 또는 저지의 필요성을 주장하기 위함이었을테고, 기업 입장에서는 그러한 온라인에서의 논란이 기업 실적에 미치는 영향이 있었는지 측정함으로써 만약 영향이 컸다면 이를 최소화하기 위한 대응 전략을 고안하기 위해서 이러한 문제에 관심을 가지게 될 것이라 생각합니다. 즉, 원인에 대한 개입과 조정이 주된 목적이라는 측면에서 GS25 를 둘러싼 이슈는 인과추론 문제로 볼 수 있을 것입니다. 

## 2.	인과적인 효과를 어떻게 정의하고 측정할 수 있을까?
-	일상 생활에서 인과관계에 대해서 누구나 각자의 방식으로 쉽게 떠올릴 수 있지만, 그걸 정량적으로 측정하는 건 또 다른 문제입니다. 따라서, 모든 사람들이 인과관계에 대해서 동일한 방식으로 이해하고 평가하기 위해서는 일종의 합의가 필요합니다.
-	가장 널리 활용되는 인과관계 프레임워크 중 하나인 잠재적 결과 프레임워크(potential outcomes framework)에서는 인과적인 효과를 원인이 있었을 때의 결과(사실; factual)와 그 원인이 없었다면 있었을 잠재적 결과(반사실; counterfactual) 간의 차이로 정의하고 있습니다.
-	하지만, 인과추론의 근본적인 문제는 잠재적 결과인 반사실을 현실에서 관찰할 수 없다는 것입니다. 이런 관점에서 인과추론 데이터 분석의 핵심은 특정 원인을 제외하고 나머지 특성들이 모두 비교 가능해서 (실제 관찰할 수 없는) 반사실에 근접할 수 있는 통제집단을 구성하는 것입니다.

## 3.	인과추론 분석 방법론 – 준실험/이중차분법
-	(실제로 어떤 원인이 있었지만) 만약 그 원인이 없었다면 있었을 처치집단의 반사실과 유사한 통제집단을 어떻게 구성할 수 있을까요? 가장 효과적인 방법은 무작위 실험(randomized controlled trial; RCT)을 수행함으로써 비교가능한 통제집단을 인위적으로 구성하는 것입니다. 하지만, 현실의 많은 문제에서 이러한 인위적인 실험이 불가능하다는 건 자명한 사실입니다. 그래서 무작위로 처치집단과 통제집단을 나누지는 않지만, 적절한 연구 디자인을 활용함으로써 비교가능한 통제집단을 구성해서 인과적인 효과를 분석할 수 있는데 이를 준실험(quasi-experiment)이라고 부릅니다.
-	현실에서 관찰할 수 없는 반사실을 유추하기 위해서는 언제나 적절한 가정이 필요합니다. 방법론에 따라 세부적인 디테일은 다르지만, 결국 가장 중요한 가정은 실제 원인이 없던 통제집단에서의 결과가 (실제 원인이 있었지만) 만약 원인이 없었다면 있었을 처치집단에서의 잠재적 결과(반사실)와 유사하고 비교가능하다는 것입니다.
-	이러한 준실험을 분석하는 대표적인 도구 중 하나가 이중차분법(difference-in-differences; DID)입니다. 인과추론을 위해서는 실제 원인이 있었을 때의 결과와 원인이 없었다면 있었을 잠재적 결과 간에는 원인이 있었다는 사실을 제외하고는 모두 유사해야 한다는 걸 다시 상기해봅시다. 이러한 차이를 (i) 시간에 따라 변하지 않는 차이(예: GS그룹의 총수 일가)와 (ii) 원인이 아닌 다른 요인에 의해서 시간에 따라 변하는 차이(예: 신규/폐점 점포, 소비환경 변화)로 나눌 수 있는데, 이를 최소화하는 것이 이중차분법의 목적입니다. 여기서, (i) 시간에 따라 변하지 않는 차이는 처치집단에서 원인이 있었던 시점 전/후의 차이를 통해서 효과적으로 배제할 수 있고, (ii) 원인이 없었더라도 시간에 따라 변하는 잠재적 결과의 변화는 (실제 원인은 없었지만 처치집단과 비교가능한) 통제집단에서의 시간에 따른 변화를 통해 간접적으로 유추할 수 있을 것입니다. 이를 종합해보면, 처치집단에서 원인이 있은 전/후의 차이에서 통제집단에서 전/후의 차이를 한 번 더 차분함으로써 “이중”차분법을 구성할 수 있게 됩니다. 이러한 이중차분법의 핵심 가정은 분석하고자 하는 원인이 있기 전까지는 처치집단과 통제집단의 시간에 따른 변화가 유사해서 서로 비교가능해야 한다는 것인데, 이를 평행 추세 가정(parallel trends assumption) 이라고 부릅니다.

## 4.	통제집단 없는 인과추론 – 시계열 분석
-	물론 통제집단 없이도 원인이 있었던 이후 기간에서의 잠재적 결과(반사실)를 효과적으로 추정할 수 있다면 충분히 인과추론이 가능합니다. 대표적인 사례가 바로 시계열 분석을 활용하는 방법입니다. 간단히 설명하자면, 원인이 있기 이전 기간의 데이터를 바탕으로 시계열 예측 모델을 만들고, 원인이 있었던 이후 기간으로 예측을 연장하여 반사실을 추론하고자 하는 접근입니다. 아래 Figure 1 은 R 의 CausalImpact 패키지를 활용한 시계열 인과 분석의 결과를 보여주고 있습니다.<sup>[1](#myfootnote1)</sup> 분석 결과, 2021년 2분기에서 시계열 예측을 통한 잠재적 결과 보다 매출이 조금 높은 걸 볼 수 있는데 통계적으로 유의하지는 않습니다.
-	하지만, 통제집단 없이 수행하는 인과추론은 많은 한계를 지닐 수 밖에 없는데, 처치집단의 과거 데이터만 가지고 미래에 있을 반사실을 정확하게 예측하는 건 쉽지 않기 때문입니다. 시계열 분석은 정규화된 변화(예: 계절 효과, 추세, 자기상관)에 대해서는 효과적으로 예측할 수 있지만, 원인 이외의 외부 환경 변화(예: 코로나19, 리테일 시장환경 변화) 등에 대해서는 다른 비교대상을 통해 간접적으로 유추할 수 있는 방법이 없기 때문에 이를 예측에 반영하는 건 한계가 있을 수 밖에 없습니다. 또한, 많은 기업들은 과거의 정보 뿐 아니라, 미래에 대한 전망을 종합적으로 고려하여 내생적으로 의사결정을 하기 때문에 과거 데이터를 활용해서 예측한 미래의 잠재적 결과가 특정 원인과 무관하다는 걸 증명하는 것도 쉽지 않은 문제입니다.
<img src="https://user-images.githubusercontent.com/41999451/130162637-af9a60bf-f226-4066-ba34-0866ab832b3d.png" width="800" height="400"/>

## 5.	통제집단의 구성 (1) GS25 vs CU
-	공정거래위원회 자료에 따르면, 2020년 기준 GS25 의 가맹점 수는 13,818개로 업계 1위이고, 그 뒤를 바짝 따르는 것이 CU 이며 가맹점 수는 13,737개로 매우 유사합니다 (2021년에는 CU 가 가맹점 수를 추월했다고 합니다). 따라서, GS25 와 비교가능한 통제집단으로서 CU 를 고려하는 건 지극히 합리적인 전략입니다. 여기서 결국 인과추론을 위해 던져야 할 가장 중요한 질문은, (실제 온라인 상에서 논란이 있었지만) 만약 논란이 없었다면 있었을 GS25 의 실적 추이와 같은 시기에 논란이 없었던 CU 의 실적 추이가 얼마나 유사한지 여부일 것입니다.
-	아래 Figure 2 는 2018년부터 2021년 2분기까지의 GS25 와 CU 의 매출액/영업이익/영업이익률의 추이를 보여주고 있는데, 두 기업의 실적 추이가 어느정도 유사한 걸 볼 수 있습니다.
<br/><br/>
![Figure 2 - Trends of GS25 and CU](https://user-images.githubusercontent.com/41999451/130146666-93f35ec4-30f7-4ecc-a982-e89d61d7d82b.jpg)

## 6.	통제집단의 구성 (2) GS25 vs 가상의 통제집단 (Synthetic Control)
-	CU 가 합리적인 통제집단이 될 수 있지만, 좀 더 나은 통제집단을 구성해볼 순 없을까요? 이때 고려할 수 있는 방법론이 바로 가상의 통제집단(Synthetic Control) 방법론입니다. 아이디어는 간단한데, 원인이 없는 시기에 통제집단들의 조합으로 처치집단에서의 추세를 모방하는 가상의 통제집단을 구성하고, 이를 통해서 원인이 있었던 시기에서의 반사실을 추정하고자 하는 인과추론 방법론입니다. 
-	여기서 GS25 (GS리테일 내 편의점 부문)의 가상의 통제집단을 구성하기 위해서, (1) 편의점 트렌드를 반영하기 위해 가맹점 수의 90% 이상을 차지하는 업계 Top 4 중 GS25 를 제외한 CU (BGF리테일), 세븐일레븐 (롯데지주 내 코리아세븐), 이마트24 (이마트 내 이마트24 부문), (2) GS리테일 그룹의 전사적인 변화를 반영하기 위해 GS리테일 내의 GS슈퍼, (3) 유통업계 트렌드를 반영하기 위해 대형마트인 이마트/롯데마트와 슈퍼마켓인 이마트 에브리데이/롯데슈퍼를 통제집단 후보(donor pool)로 설정하고, 이들의 2018년~2020년까지의 과거 실적과 자산/자본 데이터를 활용해서 가상의 통제집단을 구성했습니다. 모든 데이터는 각 기업들의 공시자료를 활용했고, 분석에는 R 의 TidySynth 패키지를 활용했습니다.<sup>[2](#myfootnote2)</sup> 아래 Figure 3 에서 볼 수 있는 것처럼, 가상의 통제집단 방법론의 또다른 장점은 GS25 뿐 아니라, 다른 기업들에 대해서도 비교가능한 통제집단을 체계적으로 구성할 수 있다는 것입니다.
<br/><br/>
![Figure 3 - Synthetic Weights](https://user-images.githubusercontent.com/41999451/130158572-bad9f3eb-a453-42a6-8c0d-f14691489db1.jpg)
- 물론 CU 와 단독 비교하는 것과 비교했을 때, 현재의 가상의 통제집단이 특별히 더 낫다고 보기는 어려울 수 도 있습니다. 연습문제이기 때문에 기초적인 수준의 데이터와 분석만 다루고 있지만, 가상의 통제집단의 퀄리티를 높이기 위해서 (i) 추가적인 리테일 기업들을 고려하거나 (예를 들어, 온라인 리테일 기업들도 GS25 실적에 중요한 역할을 할 수 있을 것입니다), (ii) 매출액/영업이익/영업이익률 뿐 아니라 기업들의 회계 지표들을 추가적으로 고려하거나, (iii) 가상의 이중차분법(synthetic difference-in-differences)<sup>[3](#myfootnote3)</sup>과 같은 향상된 방법론을 적용해볼 수 있을 것입니다.  

## 7.	모델 가정 검증 및 결과 해석
-	Figure 4 는 GS25 와 가상의 통제집단의 기업 실적 추이를 보여주고 있습니다. 분석 결과, 2021년 2분기에 GS25 의 실적이 좋지 않았던 것은 분명해 보입니다. 왜냐하면, 잠재적으로 GS25 가 얻을 수 있었던 매출과 비교했을 때 약 500억원의 매출(전체 매출의 2.8%)이 줄었기 때문입니다. 혹자는 이 결과를 활용하여 이중차분법을 적용해서 2021년 2분기에 GS25 의 매출이 줄어드는 인과적인 원인이 있었다고 결론 내릴지도 모릅니다. 하지만, 이러한 비교를 통해서 2021년 2분기에 있었던 “남혐 논란”이 그 원인이라고 주장할 수 있을까요?
<br/><br/>
![Figure 4 - Synthetic Control Results for GS25](https://user-images.githubusercontent.com/41999451/130155795-0b08f495-000b-4354-9647-2f830c85d0d4.jpg)
-	모든 통계 모델링은 모델에 대한 가정이 필요한데, 이는 연구자의 재량이기 때문에 최악의 경우 연구자의 입맛에 맞는 결과만 취사선택할 가능성도 배제할 수 없습니다. 그래서 항상 데이터 분석을 통해서 인과적인 해석을 할 때는 그 이면의 가정을 엄밀히 따져봐야 합니다. 모든 데이터 분석은 적절한 가정을 수반한다는 사실을 이해하는 것은 분석 결과를 비판적으로 받아들이는 첫걸음입니다.
-	앞서 살펴본 이중차분법의 핵심 가정을 떠올려봅시다. 이중차분법에서는 원인이 있기 전까지는 처치집단과 통제집단의 추세가 평행해야만 원인이 있은 이후의 결과의 차이를 인과관계로 해석할 수 있습니다. 하지만, Figure 4 에서 볼 수 있는 것처럼, GS25 의 매출이 가상의 통제집단을 통해 도출한 잠재적 실적보다 낮아지기 시작한 건 2020년 3분기부터였고 2021년 1분기에는 잠재적 성과 대비 더 큰 폭의 매출 감소(약 818억원)가 있었습니다. 이러한 점을 고려했을 때, 2021년 2분기를 기점으로 이중차분법을 적용하는 건 원인이 있기 이전 시점에서 추세가 평행해야 한다는 가정에 위배되는 걸 알 수 있습니다. 
-	또 다른 관점에서 해석해보자면, 관찰되는 변화가 정말 해당 시기에 발생한 특정 원인 때문인지 검증하기 위해서 (i) 해당 시기에 (원인에 영향을 받지 않았던) 다른 기업에도 이러한 변화가 나타나는지, (ii) 임의의 다른 시점을 가상의 원인(placebo)으로 보면 정말 아무런 변화가 관찰되지 않는지를 테스트할 수 있습니다. 다른 기업에 대한 분석은 뒤이어 설명하기로 하고, 2021년 2분기 이전 시기를 기준으로 가상의 통제집단을 구성하고 이중차분법을 적용해봐도 2021년 2분기를 기준으로 한 것과 유사한 결과를 도출할 수 있습니다. 따라서, 2021년 2분기에 GS25 의 매출이 유의하게 감소했던 것은 사실이지만, 이러한 매출 감소는 2021년 2분기에 있었던 특정 원인에 기인한다기 보다는 2020년 하반기부터 시작된 트렌드로 보는 편이 더 적절하지 않을까 생각합니다.
- 하지만, 기업 입장에서 중요한 건 매출 총액이 아니라, 매출원가와 판관비를 제외한 영업이익과 영업외비용까지 제외한 당기순이익입니다. 영업이익과 영업이익률(매출 대비 영업이익 비율)을 비교해보면, 매출 추이와는 달리 2020년 이후 GS25 의 영업이익과 영업이익률은 오히려 잠재적 성과 대비 더 높은 걸 볼 수 있습니다.

## 8.	편의점 업계 동향에 대한 추가 분석
-	종합적인 해석을 위해서 편의점 업계 2위와 3위인 CU 와 세븐일레븐에 대해서도 추가적인 분석을 수행해보았습니다. 아래의 Figure 5 는 CU 에 대한 가상의 통제집단 분석 결과를 보여주고 있습니다. 분석 결과, 최근 잠재적 결과 대비 CU 의 매출이 향상되고 있는 걸 볼 수 있는데, 이는 2020년 3분기부터 시작된 변화라는 걸 알 수 있습니다 (참고로, CU 의 히트상품인 “곰표”는 2020년 5월에 출시되었습니다). 특히, CU 는 2021년 2분기에 그들이 올릴 수 있었던 잠재적 성과 대비해서 매우 좋은 성과를 냈는데, 잠재적 매출 대비 약 783억원의 매출(전체 매출의 약 4.6%)이 증가했습니다. CU 의 영업이익과 영업이익률은 2021년 1분기까지는 잠재적 성과 대비 유사하거나 조금 낮았지만, 2021년 2분기에는 영업이익률도 향상되면서 수익성도 개선되고 있는 것으로 보입니다. 이는 CU 가 GS25 의 남혐 논란의 반사이익을 얻었다는 주장에 반하는 결과인데, 사람들이 기업의 수익 구조에 영향을 미칠 만큼 마진이 높은 제품군만 집중적으로 구매하지 않는 이상 매출 증가가 자연스럽게 영업이익률 증가로 이어지지는 않을 것입니다. 이러한 일시적인 매출 증가 보다는, 2021년 5월부터 주류 규제 완화로 인해 가능해진 수제맥주 OEM 생산이 CU 의 비용/수익 구조에 영향을 미쳤을 가능성이 더 높을 것이라 생각합니다.
<br/><br/>
![Figure 5 - Synthetic Control Results for CU](https://user-images.githubusercontent.com/41999451/130155798-cbeff405-dfab-4d15-adfd-4df50db03ee7.jpg)
-	아래의 Figure 6 은 현재 편의점 업계에서 가장 위기에 처한 것은 GS25 가 아니라, 세븐일레븐 일지도 모른다는 걸 보여주고 있습니다. 세븐일레븐은 2019년 말부터 잠재적 성과 대비해서 매출, 영업이익, 영업이익률이 모두 그들에게 기대되는 잠재적 실적을 하회하고 있습니다. 매출액의 실적 감소 보다 영업이익과 영업이익률의 실적 감소 규모가 상대적으로 더 큰 것으로 보아, 타 기업들과는 다른 세븐일레븐의 비효율적인 비용구조로 인해 발생하는 문제로 해석됩니다.
<br/><br/>
![Figure 6 - Synthetic Control Results for 7-Eleven](https://user-images.githubusercontent.com/41999451/130155803-4067d420-9929-4741-9558-8875e6aa5ca6.jpg)
-	이러한 데이터 분석 결과들을 종합하면 다음과 같습니다. 2020년 이후로 GS25 는 매출 신장에 어려움을 겪고 있고, CU 는 이 시기를 전화위복의 기회로 활용해서 매출을 크게 끌어올리고 있는 것으로 보입니다. 하지만, GS25 는 매출이 감소하고는 있지만 영업이익은 여느때와 크게 다르지 않고 양호한 실적을 냈는데, 이는 GS25 가 매출 증대를 위한 공격적인 투자 보다는 비용 절감에 초점을 두고 있기 때문으로 해석할 수 있습니다. 반면에, CU 는 외형 확장과 매출 증대에 힘쓰고 있고, 이로 인한 비용 증가로 인해 영업이익 측면에서는 매출 증가 만큼의 성과를 거두고 있지는 않았습니다. 하지만, CU 의 수익성은 2021년 2분기에 크게 개선되었는데 이러한 효과가 지속될지 여부는 앞으로 지켜볼 일입니다.
-	사실 편의점 업계의 앙대산맥인 GS25 와 CU 의 경쟁 양상의 변화(GS25 의 비용절감 vs. CU 의 매출증대 전략)는 2021년 2분기 보다 한참 이전에 시작되었습니다. 과거 기사들은 이러한 변화들을 잘 보여주고 있습니다 (2019년 http://news.bizwatch.co.kr/article/consumer/2019/05/14/0014; 2020년 https://www.yna.co.kr/view/AKR20200507163300030). 이러한 편의점 업계 Big 2 의 선전 속에서 3등 기업인 세븐일레븐은 비효율적인 비용구조로 인해 어려움에 직면해 있는 것으로 보입니다. 편의점 업계의 동향을 종합적으로 고려했을 때, 비즈니스적인 관점에서 우리가 주목해야 하는 건 서로 다른 전략에 따른 편의점 업계의 판도 변화와 그 사이에서 위기에 처해있는 세븐일레븐의 미래가 아닐까 생각합니다. 또한, 편의점 업계에서의 사례는 규제 완화와 위탁제조가 영세기업들, 그리고 공급사슬과 유통 구조에 미치는 영향을 분석하기 위한 흥미로운 사례가 될 수 있을 것입니다.

## 9.	메커니즘 분석의 중요성과 빅데이터
-	하지만, 위에서의 분석은 데이터 상의 한계가 많기 때문에 단정적으로 결론을 내리는 것은 지양해야 합니다. 왜냐하면, 현재 데이터 분석을 통해서 우리가 실제 분석하고 있는 원인은 “2021년 2분기”에 특정 기업에서 발생한 일련의 사건들이지, 그 때 발생했던 특정 사건의 효과만 떼어놓고 분석하는 것은 아니기 때문입니다. 예를 들어, GS25 가 보도자료를 통해 말하고 있는 것처럼 “5~6월 강수 여파” 효과도 완전히 배제할 수는 없을 것입니다. 이처럼 인과적 효과를 추정하는 것 뿐 아니라, 이를 야기하는 메커니즘을 밝히는 것도 매우 중요한 인과추론 분석입니다.
-	이러한 메커니즘 분석을 위해서는 보다 세밀한 데이터가 요구됩니다. 예를 들어, “온라인 상에서의 남혐 논란”에 대한 원인을 분석하기 위해서, 지역별/성별/연령별/업종별 검색어 트렌드나 카드사용 내역 데이터 등을 활용해서 특정 인구 집단(cohort)이 많은 지역에서 GS25 의 매출의 변화가 더 컸는지, 또는 특정 인구집단이 선호하는 상품군의 매출의 변화가 더 컸는지에 대해 분석할 수 있을 것입니다. 비슷하게, “5~6월 강수 여파”에 대한 원인을 분석하기 위해서, 지역별 강수량 데이터를 활용해서 강수량이 더 많았던 지역에서 GS25 의 매출이 더 크게 변화했는지 여부를 분석할 수 있을 것입니다.
-	인과추론 관점에서 빅데이터의 진정한 힘은 데이터 사이즈가 아닌, 보다 엄밀한 메커니즘 분석을 가능하게 하는 데이터의 세밀도(granularity)에서 나온다고 볼 수 있습니다.

## 10.	결론
-	데이터 분석과 인과추론은 합리적인 의사결정을 위해서 중요합니다. 하지만, 데이터는 정직하지만 투명하지는 않습니다. 모든 상관관계가 함께 버무려져 있는 데이터만으로 특정 요인의 인과적인 효과를 추론하는 건 현실 문제에서는 대단히 어렵기 때문에, 인과추론을 포함한 모든 데이터 분석은 필연적으로 불완전합니다. 하지만, 논리적인 연구 디자인과 모델에 대한 엄밀한 가정 검증을 통해 충분히 합리적인 인과추론을 도모할 수 있을 것입니다.
-	인과관계를 보다 합리적으로 추론하기 위해서는 잠재적 결과 프레임워크나 구조적 인과모형과 같은 다양한 인과관계 프레임워크를 이해하는 것이 중요합니다. 인과추론 서머세션(https://sites.google.com/view/causal-inference2021) 은 그러한 목적에서 기획되었으며, 총 18개의 세미나 영상들은 곧 순차적으로 유튜브(https://www.youtube.com/channel/UCkEHnPq2T8Vpafk3p-Rk49A) 에 업로드 될 예정입니다.
-	데이터 분석에 대한 피드백과 챌린지는 언제나 환영합니다. 가급적 댓글 보다는 이메일로 의견 보내주세요 (UNCG 박지용, jiyong.park@uncg.edu).
<br/><br/>


<a name="myfootnote1">1</a>: https://cran.r-project.org/web/packages/CausalImpact/vignettes/CausalImpact.html. 모델에 관한 자세한 설명은 Brodersen, K.H., Gallusser, F., Koehler, J., Remy, N. and Scott, S.L., 2015. Inferring Causal Impact using Bayesian Structural Time-Series Models. Annals of Applied Statistics, 9(1), pp.247-274. 참고.<br/>
<a name="myfootnote2">2</a>: https://github.com/edunford/tidysynth. 모델에 관한 자세한 설명은 Abadie, A., 2021. Using Synthetic Controls: Feasibility, Data Requirements, and Methodological Aspects. Journal of Economic Literature, 59(2), pp.391-425. 참고.<br/>
<a name="myfootnote3">3</a>: 모델에 관한 자세한 설명은 Arkhangelsky, D., Athey, S., Hirshberg, D.A., Imbens, G.W. and Wager, S., 2019. Synthetic Difference in Differences (No. w25532). National Bureau of Economic Research. 참고.<br/>
