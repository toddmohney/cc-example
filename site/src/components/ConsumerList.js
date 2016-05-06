import React from 'react'

const ConsumerList = () => (
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
          <tr>
            <td>Ashton</td>
            <td>16</td>
          </tr>
          <tr>
            <td>Bob</td>
            <td>4</td>
          </tr>
          <tr>
            <td>Chuck</td>
            <td>5</td>
          </tr>
        </tbody>
      </table>
    </div>
  </div>
)

export default ConsumerList

