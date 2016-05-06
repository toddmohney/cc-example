import React, { PropTypes } from 'react'
import ConsumerListItem from './ConsumerListItem'

const ConsumerList = ({ listData, onListItemClick }) => (
  <div className="panel panel-default">
    <div className="panel-heading">
      <h3 className="panel-title">Number of consumptions per consumer</h3>
    </div>
    <div className="panel-body">
      <table className="table">
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
              onConsumerClick={onListItemClick}
              onConsumerClick={() => onListItemClick(consumer.id)}
            />
          )}
        </tbody>
      </table>
    </div>
  </div>
)

ConsumerList.propTypes = {
  listData: PropTypes.array.isRequired,
  onListItemClick: PropTypes.func.isRequired
}

export default ConsumerList

