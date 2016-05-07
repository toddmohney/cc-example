import React, { PropTypes } from 'react'
import ConsumerListItem from './ConsumerListItem'
import Panel from './Panel'

const ConsumerList = ({ listData, onListItemClick, selectedListItem }) => {
  let panelTitle = "Number of consumptions per consumer";
  let tableContent = buildTable(listData, onListItemClick, selectedListItem);

  return (
    <Panel
      title={panelTitle}
      content={tableContent}
    />
  )
}

function buildTable(listData, onListItemClick, selectedListItem) {
  return (
    <table className="table tableHover">
      <thead>
        <tr>
          <th>Name</th>
          <th>Consumptions</th>
        </tr>
      </thead>
      <tbody>
        {buildTableData(listData, onListItemClick, selectedListItem)}
      </tbody>
    </table>
  );
}

function buildTableData(listData, onListItemClick, selectedListItem) {
  return listData.map(consumer =>
    <ConsumerListItem
      key={consumer.id}
      consumer={consumer}
      onConsumerClick={() => onListItemClick(consumer.id)}
      selected={selectedListItem && selectedListItem.id == consumer.id}
    />)
}

ConsumerList.propTypes = {
  listData: PropTypes.array.isRequired,
  selectedListItem: PropTypes.object,
  onListItemClick: PropTypes.func.isRequired
}

export default ConsumerList

