import React, { PropTypes } from 'react'
import Panel from './Panel'

const ConsumerDetail = ({ consumer }) => {
  let eaterName = consumer ? consumer.eater.name : "";
  let panelTitle = "Meat consumed by " + eaterName;
  let pieChartStyles = {
    width: "100%",
    height: "400px"
  };
  let panelContent = <div id="piechart" style={pieChartStyles}></div>

  if(consumer) {
    drawChart(consumer.meatbarsEaten);
  }

  return (
    <Panel
      title={panelTitle}
      content={panelContent}
    />
  );
}

function drawChart(meatbarsEaten) {
  let consumptionData = meatbarsEaten.reduce((acc, eatenMeatbar) => {
    let key = eatenMeatbar.meatbar.name;
    if(acc[key] == undefined) {
      acc[key] = 0;
    }
    acc[key] += 1;
    return acc;
  }, {});


  let chartData = Object.keys(consumptionData).reduce((acc, key) => {
    acc.push([key, consumptionData[key]]);
    return acc;
  }, [["Type", "Count"]]);

  let data = google.visualization.arrayToDataTable(chartData);
  let chart = new google.visualization.PieChart(document.getElementById('piechart'));
  let options = {
    chartArea: {
      top: '10%',
    },
    legend: {
      alignment: 'start',
      position: 'top',
      maxLines: 1
    }
  };
  chart.draw(data, options);
}

ConsumerDetail.propTypes = {
  consumer: PropTypes.object
}

export default ConsumerDetail

