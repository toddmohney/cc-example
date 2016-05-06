import React, { PropTypes } from 'react'

const ConsumerListItem = ({ consumer, onConsumerClick, selected }) => {
  let classes = selected ? "active" : "";
  return (
    <tr onClick={onConsumerClick} className={classes} >
      <td>{consumer.eater.name}</td>
      <td>{consumer.meatbarsEaten.length}</td>
    </tr>
  )
}

ConsumerListItem.propTypes = {
  selected: PropTypes.bool.isRequired,
  consumer: PropTypes.object.isRequired,
  onConsumerClick: PropTypes.func.isRequired
}

export default ConsumerListItem


