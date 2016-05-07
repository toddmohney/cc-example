import React, { PropTypes } from 'react';

const ConsumerTableRow = ({ consumer, onConsumerClick, selected }) => {
  const classes = selected ? 'active' : '';
  return (
    <tr onClick={onConsumerClick} className={classes} >
      <td>{consumer.eater.name}</td>
      <td>{consumer.meatbarsEaten.length}</td>
    </tr>
  );
};

ConsumerTableRow.propTypes = {
  selected: PropTypes.bool.isRequired,
  consumer: PropTypes.object.isRequired,
  onConsumerClick: PropTypes.func.isRequired
};

export default ConsumerTableRow;
