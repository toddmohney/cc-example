import React, { PropTypes } from 'react'
import ConsumerTableRow from './ConsumerTableRow'
import Panel from './Panel'

const ConsumerTable = ({ listData, onListItemClick, selectedListItem }) => {
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
    <ConsumerTableRow
      key={consumer.id}
      consumer={consumer}
      onConsumerClick={() => onListItemClick(consumer.id)}
      selected={selectedListItem && selectedListItem.id == consumer.id}
    />)
}

ConsumerTable.propTypes = {
  listData: PropTypes.array.isRequired,
  selectedListItem: PropTypes.object,
  onListItemClick: PropTypes.func.isRequired
}

export default ConsumerTable

