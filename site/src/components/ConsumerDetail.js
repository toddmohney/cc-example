import React, { PropTypes } from 'react';
import Panel from './Panel';

const ConsumerDetail = ({ consumer }) => {
  const eaterName = consumer ? consumer.eater.name : '';
  const panelTitle = 'Meat consumed by ' + eaterName;
  const panelContent = <div id="piechart" style={pieChartStyles}></div>;

  if (consumer) {
    drawChart(consumer.meatbarsEaten);
  }

  return <Panel title={panelTitle} content={panelContent} />;
};

function drawChart(meatbarsEaten) {
  const consumptionData = meatbarsEatenByType(meatbarsEaten);
  const chartData = Object.keys(consumptionData).reduce((acc, key) => {
    acc.push([key, consumptionData[key]]);
    return acc;
  }, [['Type', 'Count']]);

  getPieChart().draw(
    google.visualization.arrayToDataTable(chartData),
    pieChartOptions
  );
}

function getPieChart() {
  return new google.visualization.PieChart(document.getElementById('piechart'));
}

function meatbarsEatenByType(meatbarsEaten) {
  return meatbarsEaten.reduce((acc, eatenMeatbar) => {
    const key = eatenMeatbar.meatbar.name;
    acc[key] = acc[key] || 0;
    acc[key] += 1;
    return acc;
  }, {});
}

const pieChartStyles = {
  width: '100%',
  height: '400px'
};

const pieChartOptions = {
  chartArea: {
    top: '10%'
  },
  legend: {
    alignment: 'start',
    position: 'top',
    maxLines: 1
  }
};

ConsumerDetail.propTypes = {
  consumer: PropTypes.object
};

export default ConsumerDetail;
