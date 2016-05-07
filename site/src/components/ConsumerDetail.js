import React, { PropTypes } from 'react'
import Panel from './Panel'

const ConsumerDetail = ({ selectedListItem }) => {
  // var consumptionData = selectedListItem.reduce( (acc, eatenMeatbar) => {
    // let key = eatenMeatbar.eater.id;
    // acc[key] == acc[key] || [];
    // acc[key].push(eatenMeatbar);
    // return acc;
  // }, {});

  let eaterName = selectedListItem ? selectedListItem.eater.name : "";
  let panelTitle = "Meat consumed by " + eaterName;
  let panelContent = <div id="piechart"></div>

  return (
    <Panel
      title={panelTitle}
      content={panelContent}
    />
  );
}

ConsumerDetail.propTypes = {
  selectedListItem: PropTypes.object
}

export default ConsumerDetail

