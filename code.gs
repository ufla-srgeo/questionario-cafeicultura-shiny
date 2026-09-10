function doPost(e) {
  try {
    // 1. Lê os dados enviados do formulário (formato JSON)
    var dados = JSON.parse(e.postData.contents);
    
    // 2. Acessa a planilha ativa e a aba 'Respostas'
    var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName("Respostas");
    if (!sheet) {
      sheet = SpreadsheetApp.getActiveSpreadsheet().getSheets()[0];
    }
    
    // 3. Se a planilha estiver vazia, cria o CABEÇALHO NOVO (com 14 colunas)
    if (sheet.getLastRow() === 0) {
      sheet.appendRow([
        "Data/Hora", 
        "Nome",
        "Curso / Área",
        "Exp. SIG/R",
        "Q1 - Rápida/Prática (Utilidade)",
        "Q2 - Útil Gestão (Utilidade)",
        "Q3 - Navegação Intuitiva (Facilidade)",
        "Q4 - Pouco Esforço (Facilidade)",
        "Q5 - Usaria no Futuro (Intenção)",
        "Q6 - Recomendaria (Intenção)",
        "Q7 - Confiança (SUS adaptado)",
        "Radar Clareza",
        "Bugs/Travamentos",
        "Sugestões Atributos"
      ]);
      // Aplica negrito no cabeçalho (14 colunas)
      sheet.getRange(1, 1, 1, 14).setFontWeight("bold");
    }
    
    // 4. Pega os valores enviados pelo HTML e monta a linha
    var dataHora = new Date();
    
    var linha = [
      dataHora,                                      // Data/Hora
      dados.nome || "",                              // Nome
      dados.curso_area || "",                        // Curso / Área
      dados.exp_sig || "",                           // Exp. SIG/R
      dados.unified_1 || "",                         // Q1
      dados.unified_2 || "",                         // Q2
      dados.unified_3 || "",                         // Q3
      dados.unified_4 || "",                         // Q4
      dados.unified_5 || "",                         // Q5
      dados.unified_6 || "",                         // Q6
      dados.unified_7 || "",                         // Q7
      dados.radar_clareza || "",                     // Radar
      dados.bugs || "",                              // Bugs
      dados.sugestoes || ""                          // Sugestões
    ];
    
    sheet.appendRow(linha);
    
    // 5. Retorna uma resposta de sucesso
    return ContentService.createTextOutput(JSON.stringify({
      status: "sucesso",
      mensagem: "Dados salvos com sucesso!"
    })).setMimeType(ContentService.MimeType.JSON);
    
  } catch (erro) {
    // 6. Em caso de erro, retorna a mensagem para depuração
    Logger.log("Erro: " + erro.toString());
    return ContentService.createTextOutput(JSON.stringify({
      status: "erro",
      mensagem: erro.toString()
    })).setMimeType(ContentService.MimeType.JSON);
  }
}
