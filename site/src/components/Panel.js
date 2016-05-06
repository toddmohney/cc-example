import React, { PropTypes } from 'react'
import ConsumerListItem from './ConsumerListItem'

const Panel = ({ title, content }) => (
  <div className="panel panel-default">
    <div className="panel-heading">
      <h3 className="panel-title">
        {title}
      </h3>
    </div>
    <div className="panel-body">
      {content}
    </div>
  </div>
)

Panel.propTypes = {
  title: PropTypes.string.isRequired,
  content: PropTypes.object.isRequired
}

export default Panel


