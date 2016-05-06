import React, { PropTypes } from 'react'
import ConsumerListItem from './ConsumerListItem'

const ConsumerList = ({ listData, onListItemClick, selectedListItem }) => (
  <div className="panel panel-default">
    <div className="panel-heading">
      <h3 className="panel-title">Number of consumptions per consumer</h3>
    </div>
    <div className="panel-body">
      <table className="table tableHover">
        <thead>
          <tr>
            <th>Name</th>
            <th>Consumptions</th>
          </tr>
        </thead>
        <tbody>
          { listData.map(consumer =>
            <ConsumerListItem
              key={consumer.id}
              consumer={consumer}
              onConsumerClick={() => onListItemClick(consumer.id)}
              selected={selectedListItem.id == consumer.id}
            />
          )}
        </tbody>
      </table>
    </div>
  </div>
)

ConsumerList.propTypes = {
  listData: PropTypes.array.isRequired,
  selectedListItem: PropTypes.object.isRequired,
  onListItemClick: PropTypes.func.isRequired
}

export default ConsumerList

