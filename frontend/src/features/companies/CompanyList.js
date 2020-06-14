import React from 'react';
const CompanyList = ({ companies }) => (
  <table>
    <thead>
      <tr>
        <td>Comapny Name</td>
        <td>Ticker</td>
        <td>Last Close</td>
        <td>Score</td>
      </tr>
    </thead>
    <tbody>
    {
      companies.map(company => 
        <tr key={company.company.entityKey}>
          <td>{company.company.entityVal.swsCompanyName}</td>
          <td>{company.company.entityVal.swsCompanyUnique_symbol}</td>
          <td>{company.close}</td>
          <td>{company.score}</td>
        </tr>
      )
    }
    </tbody>
  </table>
)

export default CompanyList