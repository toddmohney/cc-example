import React, { PropTypes } from 'react'

const ConsumerListItem = ({ consumer, onConsumerClick }) => (
  <tr onClick={onConsumerClick} >
    <td>{consumer.eater.name}</td>
    <td>{consumer.meatbarsEaten.length}</td>
  </tr>
)

ConsumerListItem.propTypes = {
  consumer: PropTypes.object.isRequired,
  onConsumerClick: PropTypes.func.isRequired
}

export default ConsumerListItem


